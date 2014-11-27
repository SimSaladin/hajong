------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Server
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Server where

------------------------------------------------------------------------------
import           Hajong.Connections
import           Hajong.Worker
import           Mahjong

------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Control.Concurrent
import           Data.IntMap                (IntMap)
import           Data.Set                   (mapMonotonic)
import qualified Network.WebSockets         as WS
import qualified Data.Aeson                 as A
import           System.Log.FastLogger (LoggerSet)
import           Text.PrettyPrint.ANSI.Leijen (putDoc)

------------------------------------------------------------------------------

-- * Types

data Game = Game
    { _gWorker :: WorkerData
    , _gThread :: ThreadId
    , _gClients :: IntMap Client
    } deriving (Typeable)

data ServerState = ServerState
    { _seConnections   :: IntMap Client               -- ^ Client has nick
    , _seLounge        :: Set Client                  -- ^ Idle clients
    , _seNicks         :: Set Nick                    -- ^ Taken nicks
    , _seGameCounter   :: Int                         -- ^ Game counter
    , _seClientCounter :: Int                         -- ^ Client counter
    , _seWorkers       :: IntMap Game                 -- ^ Active games
    , _seWatcher       :: TChan (Int, WorkerResult)   -- ^ Worker watcher, **broadcast chan**.
    , _seLoggerSet     :: LoggerSet                   -- ^ Fed to new workers
    } deriving (Typeable)

data PartedException = PartedException deriving (Show, Typeable)
instance Exception PartedException

-- ** Lenses

--
makeLenses ''ServerState
makeLenses ''Game

gameId :: Applicative f => Nick -> LensLike' f ServerState Int
gameId nick = seConnections.at nick._Just._2._Just

gameAt :: Int -> Lens' ServerState (Maybe Game)
gameAt n = seWorkers.at n

ssVar :: Lens' (TVar ServerState, Client) (TVar ServerState)
ssVar = _1

client :: Lens' (TVar ServerState, Client) Client
client = _2

-- * Pure functions

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (seLounge.to (mapMonotonic getNick))
    <*> view (seWorkers.to (fmap $ view $ gWorker.wSettings))

-- | Remove from seLounge
clientToGame :: Int -> Client -> ServerState -> ServerState
clientToGame n c = (seLounge %~ deleteSet c)
        . (seConnections.at (getNick c)._Just._2 .~ Just n)

-- | Add a client based on nick.
tryAddClient :: Client -> ServerState -> Maybe (ServerState -> (Client, ServerState))
tryAddClient c ss

    -- Previously known client but not connected, goes maybe to a game
    | Just i  <- getIdent c
    , Nothing <- ss^.seConnections.at i
    = Just $ \ss -> (c, ss & seConnections.at i .~ Just c
                           & if' (ingame i ss) (seLounge %~ insertSet c) id)

    -- Completely new client nick, goes to lounge
    | Nothing <- getIdent c
    = Just $ \ss ->
        let (i,ss') = ss & seClientCounter <+~ 1
        in  Just (c, ss' & seConnections.at i .~ Just c
                         & seLounge %~ insertSet c)

    -- Nick taken by a real client
    | otherwise = Nothing

ingame :: Int -> ServerState -> Bool
ingame i ss = isJust $ ss^?seWorkers.folded.filtered (isJust . (^.gClients.at i))

------------------------------------------------------------------------------

-- * Entry points

-- | Invoke the 'workerWatcher' and the websocket 'app'.
runServer :: TVar ServerState -> IO ()
runServer ss_v = do
    void . forkIO $ workerWatcher ss_v
    WS.runServer "0.0.0.0" 8001 $ app ss_v

newServer :: LoggerSet -> IO (TVar ServerState)
newServer lgr = do
    chan <- newBroadcastTChanIO
    atomically . newTVar $ ServerState
        mempty mempty mempty 0 0 mempty chan lgr

-- ** Websocket app

-- | On every new request, parse the first 'Event' and check whether it is
-- a 'JoinServer' event and the nick isn't taken. Invoke 'clientListener'
-- for the client on success, otherwise close the connection with an
-- appropriate 'Invalid' event.
app :: TVar ServerState -> WS.ServerApp
app ss_v pending = do
    conn  <- WS.acceptRequest pending
    event <- WS.receiveData conn

    case A.decode event of
        Just (JoinServer nick) -> clientWorkerMain ss_v (websocketClient nick conn)
        _                      -> do
            let c = websocketClient "name-not-set" conn
            runClientWorker ss_v c $ do
                $logWarn $ "Received non-event or malformed json first: " <> tshow event
                unicast c $ Invalid "No JoinServer received. Please identify yourself."

-- * Worker Watcher

workerWatcher :: TVar ServerState -> IO ()
workerWatcher ss_v = do
    chan <- readTVarIO ss_v >>= dupTChan . _seWatcher
    forever $
        atomically (readTChanIO chan) >>= uncurry gameEnded >>= modifyTVar ss_v

-- | worker has finished
workerEnded :: Int -> WorkerResult -> IO (ServerState -> ServerState)
workerEnded game res = do
    either ($logError $ "Game " ++ tshow game ++ " errored! " ++)
           (\x -> $logInfo $ "Game " ++ tshow game ++ " finished. " ++ tshow x)
           res
    return $ \ss -> 
        let cs = ss^?!seWorkers.at game.gClients
        in ss & (seWorkers.at game .~ Nothing) . (seLounge %~ union cs)


------------------------------------------------------------------------------

-- * Client listening

newtype ClientWorker a = ClientWorker { unClientWorker :: LoggingT (ReaderT (TVar ServerState, Client) IO) a }
                         deriving ( Functor, Applicative, Monad, MonadIO
                                  , MonadReader (TVar ServerState, Client), MonadLogger)

-- ** Utility

withAtomic :: (TVar ServerState -> STM a) -> ClientWorker a
withAtomic f = view ssVar >>= atomically . f

putWorker :: Game -> WorkerInput -> ClientWorker ()
putWorker g = atomically . putTMVar (g^.gWorker.wInput)

-- | Send to everyone in lounge.
broadcast :: Event -> ClientWorker ()
broadcast event = rview ssVar >>= liftIO . broadcast' event

broadcast' :: Event -> ServerState -> IO ()
{-# INLINE broadcast' #-}
broadcast' event ss = forM_ (ss ^. seLounge) (`unicast` event)

-- ** Entry points

runClientWorker :: TVar ServerState -> Client -> ClientWorker a -> IO a
runClientWorker ss c m = runReaderT (runStdoutLoggingT (unClientWorker m)) (ss, c)

-- | After-handshake stuff
clientWorkerMain :: TVar ServerState -> Client -> IO ()
clientWorkerMain ss_v c = go connects `finally` go disconnects
  where
    go = runClientWorker ss_v c

-- ** Logic

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

            let clientInfo = ss ^. seConnections.at nick

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

    ss <- withAtomic $ \var -> do
        modifyTVar var
            $ set (seConnections.at nick._Just._1) (dummyClient nick)
            . over seLounge (deleteSet c)

        readTVar var

    case ss^?gameId (getNick c) >>= \i -> ss^.gameAt i of
        Just g  -> putWorker g $ WorkerPartPlayer c (const $ return ())
        Nothing -> return ()

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

-- ** Actions

createGame :: Text -> ClientWorker Int
createGame name = do
    wd <- WorkerData (GameSettings name)
        <$> liftIO (newTVarIO $ newEmptyGS (dummyClient "") name)
        <*> liftIO newEmptyTMVarIO
        <*> fmap _seLoggerSet (rview ssVar)
    threadId <- liftIO $ startWorker wd

    let game = Game wd threadId

    n <- withAtomic $ \v -> do
        s <- readTVar v
        let (n, s') = s & seGameCounter <+~ 1
        writeTVar v $ seWorkers %~ insertMap n game $ s'
        return n

    broadcast $ GameCreated (n, name, mempty)
    return n

joinGame :: Int -> ClientWorker ()
joinGame n = do
    ss <- rview ssVar
    c  <- view client
    let nick = getNick c

    $logInfo $ "Nick " <> nick <> " joins game " <> tshow n

    case ss^?gameId nick of
        Just n'
            | n /= n' -> unicast c $ Invalid "You are already in some game. You cannot join multiple games simultaneously."
        _             -> case ss ^. gameAt n of
            Just g -> do
                ss_v <- view ssVar
                putWorker g $ WorkerAddPlayer c $ \gs -> do
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
        Just g -> putWorker g WorkerForceStart

-- | Pass the TA to relevant worker
handleGameAction :: GameAction -> ClientWorker ()
handleGameAction action = do
    c  <- view client
    ss <- rview ssVar
    case ss ^? gameId (getNick c) of
        Just n | Just g <- ss^.gameAt n
            -> putWorker g (c `WorkerGameAction` action)
        _   -> unicast c $ Invalid "You are not in a game"

------------------------------------------------------------------------------

-- * Debugger

serverDebugger :: TVar ServerState -> IO ()
serverDebugger ss = go
  where
    go = do
        putStrLn "> "
        i <- getLine
        case asText i of
            ""  -> return ()
            "s" -> do s <- readTVarIO ss
                      mapM_ debugGameShow . itoList $ _seWorkers s
            _   -> putStrLn "Unknown command"
        go

    debugGameShow (n, g) = do
        putStrLn $ "Game: " ++ tshow n
        readTVarIO (g^.gWorker.wGame) >>= putDoc . pretty . fmap (unpack . getNick)
