{-# LANGUAGE TupleSections #-}
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
    , _seInGame        :: IntMap Int                  -- ^ Clients in-game
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

playerGameId :: Int -> Lens' ServerState (Maybe Int)
playerGameId i = seInGame.at i

clientGameId :: Client -> Lens' ServerState (Maybe Int)
clientGameId = playerGameId . getIdent

game :: Int -> Lens' ServerState (Maybe Game)
game n = seWorkers.at n

ssVar :: Lens' (TVar ServerState, Client) (TVar ServerState)
ssVar = _1

client :: Lens' (TVar ServerState, Client) Client
client = _2

-- * Pure functions

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (seLounge.to (mapMonotonic getNick))
    <*> view (seWorkers.to (fmap $ view $ gWorker.wSettings))

clientToGame :: Int -> Client -> ServerState -> ServerState
clientToGame g c ss = ss & seLounge %~ deleteSet c
                         & seInGame.at i .~ Just g
                         & seWorkers.ix g.gClients.at i .~ Just c
    where i = getIdent c

clientFromGame :: Client -> Int -> ServerState -> ServerState
clientFromGame c g ss = ss & seWorkers.ix g.gClients.at i .~ Nothing
    where i      = getIdent c

-- | Add a client based on nick.
tryAddClient :: Client -> ServerState -> Maybe (Client, ServerState)
tryAddClient c ss = do
    let mi = getIdent c
    (i, _) <- if mi > 0
                  then return (mi, ss) <* guard (isNothing $ ss^.seConnections.at mi)
                  else return $ ss & seClientCounter <+~ 1
    return $
        (c { getIdent = i },) $
        ss & seConnections.at i .~ Just c
           & maybe (seLounge %~ insertSet c) (\g -> clientToGame g c) (ss^.playerGameId i)

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
        mempty mempty mempty mempty 0 0 mempty chan lgr

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
        Just (JoinServer nick ident) -> clientWorkerMain ss_v (websocketClient nick conn){ getIdent = ident }
        _ -> do
            let c = websocketClient "name-not-set" conn
            runClientWorker ss_v c $ do
                $logWarn $ "Received non-event or malformed json first: " <> tshow event
                unicast c $ Invalid "No JoinServer received. Please identify yourself."

-- * Worker Watcher

workerWatcher :: TVar ServerState -> IO ()
workerWatcher ss_v = do
    chan <- readTVarIO ss_v >>= atomically . dupTChan . _seWatcher
    forever $ atomically (readTChan chan)
            >>= uncurry workerDied
            >>= atomically . go
            >>= enteredLounge ss_v
  where
    go f = do (s, r) <- readTVar ss_v <&> f
              writeTVar ss_v s
              return r

-- | worker has finished
workerDied :: Int -> WorkerResult -> IO (ServerState -> (ServerState, Set Client))
workerDied g res = do
    either (\x -> {- $logError -} putStrLn $ "Game " ++ tshow g ++ " errored! " ++ tshow x)
           (\x -> {- $logInfo  -} putStrLn $ "Game " ++ tshow g ++ " finished. " ++ tshow x)
           -- TODO ^ logging
           res
    return $ \ss ->
        let cs = ss^?!seWorkers.at g._Just.gClients^..folded & setFromList
            f  = ss & seWorkers.at g .~ Nothing
                    & seLounge %~ union cs
            in (f, cs)

enteredLounge :: TVar ServerState -> Set Client -> IO ()
enteredLounge ss_v cs = do
    ss <- readTVarIO ss_v
    mapM_ (\c -> broadcast' (PartGame $ getNick c) ss) cs

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
    dc <- view client
    $logInfo $ "New client " <> tshow (getIdent dc) <> " (" <> getNick dc <> ")"
    if (length (getNick dc) > 24)
        then unicast dc $ Invalid "Maximum nick length is 24 characters"
        else do
            accepted <- withAtomic $ \var -> do
                res <- readTVar var <&> tryAddClient dc
                maybe (return ()) (writeTVar var . snd) res
                return res
            case accepted of
                Nothing -> unicast dc $ Invalid "Nick already in use"
                Just (c, ss) -> do
                    unicast c (ClientIdentity $ getNick c)
                    unicast c (LoungeInfo $ buildLounge ss)
                    broadcast (liftA2 JoinServer getNick getIdent c)
                    local (client.~c) $ case ss^.clientGameId c of
                        Nothing -> return ()
                        Just g  -> handleEventOf c (JoinGame g (getNick c))
                    talkClient

-- | Client disconnect rituals.
disconnects :: ClientWorker ()
disconnects = do
    c <- view client
    $logInfo $ "Client disconnected (" <> tshow (getIdent c) <> ")"

    wasInGame <- withAtomic $ \var -> do
        s <- readTVar var
        let mg = s^.clientGameId c
            s' = s & seConnections.at (getIdent c) .~ Nothing
                   & seLounge %~ deleteSet c
                   & maybe id (clientFromGame c) mg
        writeTVar var s'
        return $ mg >>= \g -> s^.game g

    case wasInGame of
        Just g  -> putWorker g $ WorkerPartPlayer c (const $ return ())
        Nothing -> return ()
    broadcast (PartServer $ getNick c)

talkClient :: ClientWorker ()
talkClient = do
    c <- view client
    forever $ receive c >>= handleEventOf c

handleEventOf :: Client -> Event -> ClientWorker ()
handleEventOf c event = case event of
    JoinServer{}      -> unicast c (Invalid "Already joined (and nick change not implemented)")
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

    n <- withAtomic $ \v -> do
        (n, s) <- readTVar v <&> seGameCounter <+~ 1
        writeTVar v s >> return n

    chan     <- rview ssVar <&> _seWatcher
    threadId <- let die = atomically . writeTChan chan . (n,)
                    in liftIO $ startWorker die wd

    c <- view client
    let g = Game wd threadId (singletonMap (getIdent c) c)
    rmodify ssVar (seWorkers.at n .~ Just g)

    broadcast $ GameCreated (n, name, g^..gClients.folded <&> getNick)
    return n

joinGame :: Int -> ClientWorker ()
joinGame n = do
    ss <- rview ssVar
    c  <- view client
    let nick = getNick c

    $logInfo $ "Nick " <> nick <> " joins game " <> tshow n

    case ss^.clientGameId c of
        Just n'
            | n /= n' -> unicast c $ Invalid "You are already in some game. You cannot join multiple games simultaneously."
        _             -> case ss ^. game n of
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
    case ss^.game n of
        Nothing -> return ()
        Just g -> putWorker g WorkerForceStart

-- | Pass the TA to relevant worker
handleGameAction :: GameAction -> ClientWorker ()
handleGameAction action = do
    c  <- view client
    ss <- rview ssVar
    case ss ^. clientGameId c of
        Just n | Just g <- ss^.game n
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
