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
import           Control.Concurrent (ThreadId)
import           Data.Set                   (mapMonotonic)
import qualified Network.WebSockets         as WS
import qualified Data.Aeson                 as A
import           System.Log.FastLogger (LoggerSet)
import           Text.PrettyPrint.ANSI.Leijen (putDoc)

------------------------------------------------------------------------------

-- * Server main

data ServerState = ServerState
                 { _serverConnections :: Map Nick (Client, Maybe Int)
                 , _serverLounge :: Set Client
                 , _serverGames :: Map Int Game
                 , _serverCounter :: Int -- ^ Game counter
                 , _serverLoggerSet :: LoggerSet
                 } deriving (Typeable)

data Game = Game
          { _gWorker :: WorkerData
          , _gThread :: ThreadId
          }

data PartedException = PartedException deriving (Show, Typeable)
instance Exception PartedException

-- ** Lenses

--
makeLenses ''ServerState
makeLenses ''Game

gameId :: Applicative f => Nick -> LensLike' f ServerState Int
gameId nick = serverConnections.at nick._Just._2._Just

gameAt :: Int -> Lens' ServerState (Maybe Game)
gameAt n = serverGames.at n

ssVar :: Lens' (TVar ServerState, Client) (TVar ServerState)
ssVar = _1

client :: Lens' (TVar ServerState, Client) Client
client = _2

-- ** Utility

broadcast' :: Event -> ServerState -> IO ()
broadcast' event ss = forM_ (ss ^. serverLounge) (`unicast` event)

-- ** Entry points

newServer :: LoggerSet -> IO (TVar ServerState)
newServer = atomically . newTVar . ServerState mempty mempty mempty 0

-- | Starts websocket stuff
runServer :: TVar ServerState -> IO ()
runServer = WS.runServer "0.0.0.0" 8001 . serverApp

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

------------------------------------------------------------------------------

-- * ClientWorker

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

-- ** Entry points

runClientWorker :: TVar ServerState -> Client -> ClientWorker a -> IO a
runClientWorker ss c m = runReaderT (runStdoutLoggingT (unClientWorker m)) (ss, c)

-- | After-handshake stuff
clientWorkerMain :: TVar ServerState -> Client -> IO ()
clientWorkerMain ss_v c = go connects `finally` go disconnects
  where
    go = runClientWorker ss_v c

-- ** Client care

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

    ss <- withAtomic $ \var -> do
        modifyTVar var
            $ set (serverConnections.at nick._Just._1) (dummyClient nick)
            . over serverLounge (deleteSet c)

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

-- ** Games

createGame :: Text -> ClientWorker Int
createGame name = do
    wd <- WorkerData (GameSettings name)
        <$> liftIO (newTVarIO $ newEmptyGS (dummyClient "") name)
        <*> liftIO newEmptyTMVarIO
        <*> fmap _serverLoggerSet (rview ssVar)
    threadId <- liftIO $ startWorker wd

    let game = Game wd threadId

    n <- withAtomic $ \v -> do
        s <- readTVar v
        let (n, s') = s & serverCounter <+~ 1
        writeTVar v $ serverGames %~ insertMap n game $ s'
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

-- * Pure functions

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (serverLounge.to (mapMonotonic getNick))
    <*> view (serverGames.to (fmap $ view $ gWorker.wSettings))

-- | Remove from serverLounge
clientToGame :: Int -> Client -> ServerState -> ServerState
clientToGame n c = (serverLounge %~ deleteSet c)
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
                      mapM_ debugGameShow . itoList $ _serverGames s
            _   -> putStrLn "Unknown command"
        go

    debugGameShow (n, g) = do
        putStrLn $ "Game: " ++ tshow n
        readTVarIO (g^.gWorker.wGame) >>= putDoc . pretty . fmap (unpack . getNick)
