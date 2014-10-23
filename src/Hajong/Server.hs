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

import           Control.Monad.Logger
import           Data.Set                   (mapMonotonic)
import           Data.Default
import qualified Network.WebSockets         as WS
import qualified Data.Aeson                 as A
import          System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)

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

data PartedException = PartedException deriving (Show, Typeable)

instance Default ServerState where def = ServerState mempty mempty mempty 0
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

tryAddClient :: Client -> ServerState -> Maybe (ServerState -> ServerState)
tryAddClient c ss = case ss ^. serverConnections.at nick of
    -- Completely new client nick, goes to lounge
    Nothing -> Just $ over serverConnections (insertMap nick (c, Nothing))
                    . over serverLounge (insertSet c)
    -- Previously known client, maybe to game
    Just (sc, sg) | not (isReal sc) -> Just
        $ set (serverConnections.at nick._Just._1) c
        . (if isNothing sg then over serverLounge (insertSet c) else id)
    -- Nick taken by a real client
    _ -> Nothing
    where
        nick = getNick c

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
        Just (JoinServer nick) -> clientWorkerMain ss_v (websocketClient nick conn)
        _                      -> do
            let c = websocketClient "name-not-set" conn
            runClientWorker ss_v c $ do
                $logWarn $ "Received non-event or malformed json first: " <> tshow event
                unicast c $ Invalid "No JoinServer received. Please identify yourself."

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
    let nick = getNick c
    $logInfo $ "New client " <> nick

    accepted <- withAtomic $ \var -> do
        ss     <- readTVar var
        let res = ($ ss) <$> tryAddClient c ss
        maybe (return ()) (writeTVar var) res
        return res

    case accepted of
        Just ss -> do
            unicast c (ClientIdentity nick)
            unicast c (LoungeInfo $ buildLounge ss)
            broadcast (JoinServer nick)

            let clientInfo = ss ^. serverConnections.at nick

            reconnectGame clientInfo
            talkClient
        Nothing -> unicast c $ Invalid "Nick already in use"
  where
    reconnectGame (Just (c, Just g)) = handleEventOf c $ JoinGame g (getNick c)
    reconnectGame _                  = return ()

-- | Client disconnect rituals.
disconnects :: ClientWorker ()
disconnects = do
    c <- view client
    let nick = getNick c
    $logInfo $ "Client disconnected (" <> nick <> ")"

    withAtomic $ \var -> do

        -- inform worker
        ss <- readTVar var
        case (ss ^? gameId (getNick c)) >>= \gid -> ss ^. gameAt gid of
            Just (wi_v,_,_) -> putTMVar wi_v $ WorkerPartPlayer c (const $ return ())
            Nothing         -> return ()

        modifyTVar var
            $ set (serverConnections.at nick._Just._1) (dummyClient nick)
            . over serverLounge (deleteSet c)
            . over (serverGames.each._3) (deleteSet c)

    broadcast (PartServer nick)

talkClient :: ClientWorker ()
talkClient = do
    c <- view client
    forever $ receive c >>= handleEventOf c

handleEventOf :: Client -> Event -> ClientWorker ()
handleEventOf c event = case event of
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
        $logWarn $ "Ignored event: " <> tshow event

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

    let gs = newEmptyGS (dummyClient "(nobody)") name
    _threadId <- liftIO $ startWorker wvar gs =<< newStdoutLoggerSet defaultBufSize
    broadcast $ GameCreated (n, name, mempty)
    return n

joinGame :: Int -> ClientWorker ()
joinGame n = do
    ss <- rview ssVar
    c  <- view client
    let nick = getNick c

    $logDebug $ nick <> " joining game " <> tshow n

    case ss^?gameId nick of
        Just n'
            | n /= n' -> unicast c $ Invalid "You are already in some game. You cannot join multiple games simultaneously."
        _             -> case ss ^. gameAt n of
            Just (wi_v,_,_) -> do
                ss_v <- view ssVar
                atomically $ putTMVar wi_v $ WorkerAddPlayer c $ \gs -> do
                    multicast gs (JoinGame n nick)
                    ss' <- atomically $ modifyTVar ss_v (clientToGame n c) >> readTVar ss_v
                    broadcast' (JoinGame n nick) ss'
            Nothing         -> unicast c $ Invalid "Game not found"

-- | Force the specfied game to start even if there are not enough players.
forceStart :: Int -> ClientWorker ()
forceStart n = do
    ss <- rview ssVar
    case ss^.gameAt n of
        Nothing -> return ()
        Just (wi_v,_,_) -> atomically $ putTMVar wi_v WorkerForceStart

-- | Pass the TA to relevant worker
handleGameAction :: GameAction -> ClientWorker ()
handleGameAction action = do
    c  <- view client
    ss <- rview ssVar
    case ss ^? gameId (getNick c) of
        Just n | Just (wi_v, _, _) <- ss ^. gameAt n
            -> atomically $ putTMVar wi_v (c `WorkerGameAction` action)
        _   -> unicast c $ Invalid "You are not in a game"
