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

import           Control.Monad.Reader       (runReaderT, ReaderT)
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
                 , _serverGames :: Map Int GameInfo
                 , _serverCounter :: Int
                 }

type GameInfo = (TMVar WorkerInput, Text, Set Client)

data PartedException = PartedException deriving (Show, Typeable)
instance Exception PartedException

makeLenses ''ServerState

gameId :: Applicative f => Nick -> (Int -> f Int) -> ServerState -> f ServerState
gameId nick = serverConnections.at nick._Just._2._Just

gameAt :: Functor f => Int -> (Maybe GameInfo -> f (Maybe GameInfo)) -> ServerState -> f ServerState
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
    ss <- atomically $ newTVar newServerState
    WS.runServer "0.0.0.0" 9160 $ serverApp ss

serverApp :: TVar ServerState -> WS.ServerApp
serverApp ss_v pending = do
    conn  <- WS.acceptRequest pending
    event <- WS.receiveData conn

    case event of
        JoinServer nick -> do
            let client = websocketClient nick conn
                run    = runServer ss_v client
            putStrLn $ "New client " <> nick
            run onClientConnect `finally` run onClientDisconnect

        _ -> do
            putStrLn $ pack $ "Received non-event first: " <> show event
            unicast (websocketClient "" conn)
                    (Invalid "No JoinServer received. Please identify yourself.")

-- | Execute usual connection rituals with the new connection.
onClientConnect :: Server ()
onClientConnect = do
    c <- viewClient
    accepted <- withSSAtomic $ \var -> do
        ss <- readTVar var
        case ss ^. serverConnections.at (getNick c) of
            Nothing ->
                let newState = ss
                        & over serverConnections (insertMap (getNick c) (c, Nothing))
                        . over serverLounge (insertSet c)
                    in writeTVar var newState >> return (Just newState)
            Just _ -> return Nothing -- nick taken
    case accepted of
        Nothing -> unicast c $ Invalid "Nick already in use"
        Just ss -> do
            unicast c $ LoungeInfo $ buildLounge ss
            broadcast ss (JoinServer (getNick c))
            talkClient

-- | Client disconnect rituals.
onClientDisconnect :: Server ()
onClientDisconnect = do
    c <- viewClient
    liftIO $ putStrLn $ "Client disconnected (" <> getNick c <> ")"
    ss <- withSSAtomic $ \var -> do
        modifyTVar var
            $ over serverConnections     (deleteMap $ getNick c)
            . over serverLounge          (deleteSet c)
            -- TODO Inform the worker
            -- . over (serverGames.each.gamePlayers.each._2)
            -- (\x -> if x == Just c then Nothing else x)
        readTVar var
    broadcast ss $ PartServer (getNick c)

talkClient :: Server ()
talkClient = do
    c <- viewClient
    forever $ do
        event <- receive c
        case event of
            JoinServer _      -> unicast c (Invalid "Already joined (and nick change not implemented)")
            PartServer reason -> rview _1 >>= (`broadcast` Message "" ("User " <> getNick c <> " has left [" <> reason <> "]"))
                                    >> liftIO (throwIO PartedException)

            Message _ msg     -> rview _1 >>= (`broadcast` Message (getNick c) msg)
            CreateGame name   -> createAndJoinGame name
            JoinGame n _      -> joinGame n c
            InGameAction a    -> handleGameAction a
            _ -> do
                unicast c $ Invalid "Event not allowed or not implemented."
                liftIO $ print $ "[ignored event] " <> show event

-- * Functions

-- | Run the Server action.
runServer :: TVar ServerState -> Client -> Server a -> IO a
runServer tvar client sa = runReaderT sa (tvar, client)

-- | Send to everyone in lounge.
broadcast :: MonadIO m => ServerState -> Event -> m ()
broadcast ss event = liftIO $ forM_ (ss ^. serverLounge) (`unicast` event)

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (serverLounge . to (mapMonotonic getNick)) -- Idle
    <*> (^.serverGames.to (map $ (,) <$> view _2 <*> (^._3.to (mapMonotonic getNick))))

-- * Game workers

createAndJoinGame :: Text -> Server ()
createAndJoinGame name = do
    c <- viewClient
    n <- createGame name
    joinGame n c

createGame :: Text -> Server Int
createGame name = do
    wvar <- liftIO newEmptyTMVarIO

    n <- withSSAtomic $ \var -> do
        ss <- readTVar var
        let counter = ss ^. serverCounter
            ss' = ss & (serverCounter +~ 1)
                     & (serverGames %~ insertMap counter (wvar, name, mempty))
        writeTVar var ss'
        return counter

    liftIO $ startWorker wvar $ newEmptyGS name
    rview _1 >>= (`broadcast` GameCreated (n, name, mempty))
    return n

joinGame :: Int -> Client -> Server ()
joinGame n c = do
    ss_v <- view _1
    ss   <- rview _1

    let inLounge = ss^.serverLounge.to (member c)

    case ss^.gameAt n of
        _ | not inLounge -> unicast c $ Invalid "Already in a game. You cannot join multiple games"
        Nothing          -> unicast c $ Invalid "Game not found"
        Just (wi_v,_,_)  ->
            let action = WorkerAction $ workerAddPlayer c $ \gs -> do
                    atomically $ modifyTVar ss_v (clientToGame n c)
                    multicast gs (JoinGame n $ getNick c)
                    broadcast ss (JoinGame n $ getNick c)

                in atomically $ putTMVar wi_v action

-- | Pass the TA to relevant worker
handleGameAction :: GameAction -> Server ()
handleGameAction ga = do
    c <- viewClient
    ss <- rview _1

    case ss ^? gameId (getNick c) of
        Just n
            | Just (wi_v,_,_) <- ss ^. gameAt n
            -> atomically . putTMVar wi_v $ WorkerClientAction c ga
        _   -> unicast c $ Invalid "You are not in a game"

clientToGame :: Int -> Client -> ServerState -> ServerState
clientToGame n c = (serverLounge %~ deleteSet c)
        . (serverGames.at n._Just._3 %~ insertSet c)
        . (serverConnections.at (getNick c)._Just._2 .~ Just n)
