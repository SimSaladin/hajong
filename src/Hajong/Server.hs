{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Server
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Server where

import           ClassyPrelude              hiding (finally)
import           Control.Lens
import           Control.Monad.Reader       (runReaderT, ReaderT, MonadReader)
import           Control.Monad.Trans.Either
import           Data.Set                   (mapMonotonic)
import qualified Network.WebSockets         as WS

----------------------------------------------------
import Hajong.Connections
import Hajong.Worker
import Hajong.Game

type Server = ReaderT (TVar ServerState, Client) IO

-- * ServerState

data ServerState = ServerState
                 { _serverConnections :: Map Nick (Client, Maybe Int)
                 , _serverLounge :: Set Client
                 , _serverGames :: Map Int (TMVar WorkerInput)
                 , _serverCounter :: Int
                 }

data PartedException = PartedException deriving (Show, Typeable)
instance Exception PartedException

makeLenses ''ServerState

gameId :: Applicative f => Nick -> (Int -> f Int) -> ServerState -> f ServerState
gameId nick = serverConnections.at nick._Just._2._Just

gameAt :: Functor f => Int -> (Maybe (TMVar WorkerInput) -> f (Maybe (TMVar WorkerInput))) -> ServerState -> f ServerState
gameAt n = serverGames.at n

newServerState :: ServerState
newServerState = ServerState mempty mempty mempty 0

viewSS :: Server (TVar ServerState)
viewSS = view _1

-- | Read the ServerState TVar
viewSS' :: Server ServerState
viewSS' = viewSS >>= liftIO . readTVarIO

-- | The endpoint client
viewClient :: Server Client
viewClient = view _2

withSSAtomic :: (TVar ServerState -> STM a) -> Server a
withSSAtomic f = viewSS >>= atomically . f

-- * App

serverMain :: IO ()
serverMain = do
    state <- atomically $ newTVar newServerState
    WS.runServer "0.0.0.0" 9160 $ serverApp state

serverApp :: TVar ServerState -> WS.ServerApp
serverApp stateVar pending = do
    conn  <- WS.acceptRequest pending
    event <- WS.receiveData conn

    case event of
        JoinServer nick -> do
            let client = websocketClient nick conn
                run    = runServer stateVar client
            putStrLn $ "New client " <> nick
            run onClientConnect `finally` run onClientDisconnect

        _ -> do
            putStrLn $ pack $ "Received non-event first: " <> show event
            unicast (websocketClient "" conn)
                    (Invalid "No JoinServer received. Please identify yourself.")

-- | Execute usual connection rituals with the new connection.
onClientConnect :: Server ()
onClientConnect = do
    client <- viewClient
    accepted <- withSSAtomic $ \var -> do
        state <- readTVar var
        case state ^. serverConnections.at (getNick client) of
            Nothing ->
                let newState = state
                        & over serverConnections (insertMap (getNick client) (client, Nothing))
                        . over serverLounge (insertSet client)
                    in writeTVar var newState >> return (Just newState)
            Just _ -> return Nothing -- nick taken
    case accepted of
        Just s  -> do
            unicast client $ LoungeInfo $ buildLounge s
            broadcast (JoinServer (getNick client)) s
            talkClient
        Nothing -> unicast client nickTaken
    where
       nickTaken = Invalid "Nick already in use"

-- | Client disconnect rituals.
onClientDisconnect :: Server ()
onClientDisconnect = do
    client <- viewClient
    liftIO $ putStrLn $ "Client disconnected (" <> getNick client <> ")"
    state <- withSSAtomic $ \var -> do
        modifyTVar var
            $ over serverConnections     (deleteMap $ getNick client)
            . over serverLounge          (deleteSet client)
            -- TODO
            -- . over (serverGames.each.gamePlayers.each._2) (\x -> if x == Just client then Nothing else x)
        readTVar var
    broadcast (PartServer (getNick client)) state

talkClient :: Server ()
talkClient = do
    client <- viewClient
    forever $ do
        event <- receive client
        case event of
            JoinServer _      -> unicast client (Invalid "Already joined (and nick change not implemented)")
            PartServer reason -> viewSS' >>= broadcast (Message "" $ "User " <> getNick client <> " has left [" <> reason <> "]") >> liftIO (throwIO PartedException)
            Message _ msg     -> viewSS' >>= broadcast (Message (getNick client) msg)
            CreateGame name   -> createGame name
            JoinGame n _      -> joinGame n
            InGameAction a    -> handleGameAction a
            _ -> do
                unicast client (Invalid "Event not allowed or not implemented.")
                liftIO $ print $ "[ignored event] " <> show event

-- * Functions

-- | Run the Server action.
runServer :: TVar ServerState -> Client -> Server a -> IO a
runServer tvar client sa = runReaderT sa (tvar, client)

-- | Send to everyone in lounge.
broadcast :: MonadIO m => Event -> ServerState -> m ()
broadcast event state = liftIO $ forM_ (state ^. serverLounge) (`unicast` event)

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (serverLounge . to (mapMonotonic getNick)) -- Idle
    <*> over each (const "") . view serverGames

-- * Game workers

createGame :: Text -> Server ()
createGame name = do
    client <- viewClient
    wvar   <- liftIO newEmptyTMVarIO
    res    <- withSSAtomic $ \var -> do
        state <- readTVar var

        let counter  = state ^. serverCounter
            newState = state
                & over serverGames (insertMap counter wvar)
                . over serverCounter (+1)
                . over serverLounge (deleteSet client)

        if state ^. serverLounge.to (member client)
            then do
                writeTVar var newState
                return $ Just (newState, counter)
            else return Nothing

    case res of
        Just (state, counter) -> do
            let Just gs = newEmptyGS name & addClient client -- "Just" because starts with empty players
            liftIO $ startWorker wvar gs
            broadcast (GameCreated (counter, name, singletonSet $ getNick client)) state
            unicast client $ JoinGame counter $ getNick client
        Nothing -> unicast client $ Invalid "Already in a game or waiting for one, cannot create new one"

-- | Pass the TA to relevant worker
handleGameAction :: GameAction -> Server ()
handleGameAction ta = do
    client   <- viewClient
    received <- withSSAtomic $ \var -> do
        state <- readTVar var
        case state ^? gameId (getNick client) of
            Just n | Just wiv <- state ^. gameAt n
                -> tryPutTMVar wiv $ WorkerClientAction client ta
            _   -> return False

    -- TODO receive or not notify
    return ()

joinGame :: Int -> Server ()
joinGame n = do
    client <- viewClient
    ssVar <- view _1

    res <- withSSAtomic $ \var -> runEitherT $ do
        state <- lift $ readTVar var
        when (state^.serverLounge.to (not . member client)) $ left "Already in a game. You cannot join multiple games"
        wiv <- maybeToEitherT "Game not found" $ state^.gameAt n
        return (wiv, state)

    clientEither client res $ \(wiv, ss) -> do
        wentIn <- lift $ atomically $ tryPutTMVar wiv $ WorkerAction $

            workerAddPlayer client $ \gs -> do
                let ss' = ss & serverLounge %~ deleteSet client
                atomically $ writeTVar ssVar ss'
                multicast gs (JoinGame n $ getNick client)
                broadcast (JoinGame n $ getNick client) ss'

        return () -- TODO use wentIn?
