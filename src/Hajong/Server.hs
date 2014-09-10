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
import           Control.Monad.Logger
import           Data.Set                   (mapMonotonic)
import           Data.Default
import qualified Network.WebSockets         as WS
import qualified Data.Aeson                 as A

----------------------------------------------------
import Hajong.Connections
import Hajong.Worker
import Mahjong

-- * Common ServerState

data ServerState = ServerState
                 { _serverConnections :: Map Nick (Client, Maybe Int)
                 , _serverLounge :: Set Client
                 , _serverGames :: Map Int GameInfo
                 , _serverCounter :: Int
                 }

type GameInfo = (TMVar WorkerInput, Text, Set Client)

instance Default ServerState where
    def = ServerState mempty mempty mempty 0

data PartedException = PartedException deriving (Show, Typeable)
instance Exception PartedException

-- ** Lenses

--
makeLenses ''ServerState

gameId :: Applicative f => Nick -> LensLike' f ServerState Int
gameId nick = serverConnections.at nick._Just._2._Just

gameAt :: Int -> Lens' ServerState (Maybe GameInfo)
gameAt n = serverGames.at n

ssVar :: Lens' (TVar ServerState, Client) (TVar ServerState)
ssVar = _1

client :: Lens' (TVar ServerState, Client) Client
client = _2

-- ** Functions

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (serverLounge . to (mapMonotonic getNick)) -- Idle
    <*> (^.serverGames.to (map $ (,) <$> view _2 <*> (^._3.to (mapMonotonic getNick))))

clientToGame :: Int -> Client -> ServerState -> ServerState
clientToGame n c = (serverLounge %~ deleteSet c)
        . (serverGames.at n._Just._3 %~ insertSet c)
        . (serverConnections.at (getNick c)._Just._2 .~ Just n)

-- * ClientWorker

newtype ClientWorker a = ClientWorker { unClientWorker :: LoggingT (ReaderT (TVar ServerState, Client) IO) a }
                         deriving ( Functor, Applicative, Monad, MonadIO
                                  , MonadReader (TVar ServerState, Client), MonadLogger)

runClientWorker :: TVar ServerState -> Client -> ClientWorker a -> IO a
runClientWorker ss c m = runReaderT (runStdoutLoggingT (unClientWorker m)) (ss, c)

withAtomic :: (TVar ServerState -> STM a) -> ClientWorker a
withAtomic f = view ssVar >>= atomically . f

-- * Main

serverMain :: IO ()
serverMain = do
    ss <- atomically $ newTVar def
    WS.runServer "0.0.0.0" 9160 $ serverApp ss

-- | The websocket app
serverApp :: TVar ServerState -> WS.ServerApp
serverApp ss_v pending = do
    conn  <- WS.acceptRequest pending
    event <- WS.receiveData conn

    case A.decode event of
        Just (JoinServer nick) -> do
            putStrLn $ "New client " <> nick
            clientWorkerMain ss_v (websocketClient nick conn)

        _ -> do
            putStrLn $ pack $ "Received non-event or malformed json first: " <> show event
            unicast (websocketClient "" conn)
                    (Invalid "No JoinServer received. Please identify yourself.")

-- * ClientWorkers

-- | After-handshake stuff
clientWorkerMain :: TVar ServerState -> Client -> IO ()
clientWorkerMain ss_v c = go connects `finally` go disconnects
    where
        go = runClientWorker ss_v c

-- | Execute usual connection rituals with the new connection.
connects :: ClientWorker ()
connects = do
    c <- view client
    accepted <- withAtomic $ \var -> do
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
            broadcast (JoinServer (getNick c))
            talkClient

-- | Client disconnect rituals.
disconnects :: ClientWorker ()
disconnects = do
    c <- view client
    logInfoN $ "Client disconnected (" <> getNick c <> ")"

    withAtomic $ \var -> do

        -- inform worker
        ss <- readTVar var
        case (ss ^? gameId (getNick c)) >>= \gid -> ss ^. gameAt gid of
            Just (wi_v,_,_) -> putTMVar wi_v (workerPartPlayer c $ const $ return ())
            Nothing         -> return ()

        modifyTVar var
            $ over serverConnections     (deleteMap $ getNick c)
            . over serverLounge          (deleteSet c)
            . over (serverGames.each._3) (deleteSet c)

    broadcast $ PartServer (getNick c)

talkClient :: ClientWorker ()
talkClient = do
    c <- view client
    forever $ do
        event <- receive c
        case event of
            JoinServer _      -> unicast c (Invalid "Already joined (and nick change not implemented)")
            PartServer reason -> do
                broadcast $ Message "" ("User " <> getNick c <> " has left [" <> reason <> "]")
                liftIO (throwIO PartedException)

            Message _ msg     -> broadcast $ Message (getNick c) msg
            CreateGame name   -> createGame name >>= joinGame
            JoinGame n _      -> joinGame n
            ForceStart n      -> forceStart n
            InGameAction a    -> handleGameAction a
            _ -> do
                unicast c $ Invalid "Event not allowed or not implemented."
                logWarnN $ "Ignored event: " <> tshow event

-- | Send to everyone in lounge.
broadcast :: Event -> ClientWorker ()
broadcast event = rview ssVar >>= liftIO . broadcast' event

broadcast' :: Event -> ServerState -> IO ()
broadcast' event ss = forM_ (ss ^. serverLounge) (`unicast` event)

-- ** Games

createGame :: Text -> ClientWorker Int
createGame name = do
    wvar <- liftIO newEmptyTMVarIO

    n <- withAtomic $ \var -> do
        ss <- readTVar var
        let counter = ss ^. serverCounter
            ss' = ss & (serverCounter +~ 1)
                     & (serverGames %~ insertMap counter (wvar, name, mempty))
        writeTVar var ss'
        return counter

    let gs = newEmptyGS dummyClient name
    threadId <- liftIO $ startWorker wvar gs runStdoutLoggingT
    broadcast $ GameCreated (n, name, mempty)
    return n

joinGame :: Int -> ClientWorker ()
joinGame n = do
    c    <- view client
    ss   <- rview ssVar

    let inLounge = ss^.serverLounge.to (member c)

    logDebugN $ getNick c <> " joining game " <> tshow n

    case ss^.gameAt n of
        _ | not inLounge -> unicast c $ Invalid "Already in a game. You cannot join multiple games"
        Nothing          -> unicast c $ Invalid "Game not found"
        Just (wi_v,_,_)  -> do
            ss_v <- view ssVar
            atomically $ putTMVar wi_v $ workerAddPlayer c $ \gs -> do
                multicast gs (JoinGame n $ getNick c)
                ss' <- atomically $ do
                    modifyTVar ss_v (clientToGame n c)
                    readTVar ss_v
                broadcast' (JoinGame n $ getNick c) ss'

-- | Force the specfied game to start even if there are not enough players.
forceStart :: Int -> ClientWorker ()
forceStart n = do
    ss <- rview ssVar
    case ss^.gameAt n of
        Nothing -> return ()
        Just (wi_v,_,_) -> atomically $ putTMVar wi_v workerForceStart

-- | Pass the TA to relevant worker
handleGameAction :: GameAction -> ClientWorker ()
handleGameAction ga = do
    c <- view client
    ss <- rview ssVar
    case ss ^? gameId (getNick c) of
        Just n
            | Just (wi_v,_,_) <- ss ^. gameAt n
            -> atomically . putTMVar wi_v $ workerPartPlayer c (const $ return ())
        _   -> unicast c $ Invalid "You are not in a game"
