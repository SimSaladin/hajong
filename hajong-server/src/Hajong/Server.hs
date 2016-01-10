{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Server
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This module defines ACID game server state,
-- and WebSocket input layer.
------------------------------------------------------------------------------
module Hajong.Server where

------------------------------------------------------------------------------
import           Mahjong
import           Hajong.Server.Config
import           Hajong.Server.Internal
import           Hajong.Database
import           Hajong.Connections
import           Hajong.Client
import           Hajong.Worker
------------------------------------------------------------------------------
import           Prelude (read)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control        (MonadBaseControl(..))
import           Control.Concurrent
import           Data.Acid                          (AcidState, openLocalState)
import           Data.Acid.Remote
import           Data.Set                           (mapMonotonic)
import qualified Network.WebSockets              as WS
import qualified Data.UUID                       as UUID
import           Network
import           System.Log.FastLogger
import           System.Directory (removeFile, doesDirectoryExist, createDirectory)
import           System.Random
import           Text.PrettyPrint.ANSI.Leijen (putDoc)
------------------------------------------------------------------------------

-- | Open and read the config at config/settings.yml or $1
serverMain :: IO ()
serverMain = do
    args <- getArgs
    let configFile | [x] <- args = unpack x
                   | otherwise   = "config/settings.yml"

    config <- readConfigFile configFile (Just "hajong")
    st <- serverStartParts config
    runServer st serverDebugger

-- |
-- * set up logging
-- * restart hibernated games
-- * fork a websocket listener
-- * fork a acid remote at the socket
serverStartParts :: ServerConfig -> IO ServerSt
serverStartParts conf@ServerConfig{..} = do
    unlessM (doesDirectoryExist _logDirectory) (createDirectory _logDirectory)
    lgr                   <- newFileLoggerSet defaultBufSize $ _logDirectory </> "server.log"
    st                    <- makeServerSt conf lgr

    runServer st restartGames
    void . forkIO $ runServer st workerWatcher
    void . forkIO $ WS.runServer _websocketHost _websocketPort (wsApp st)
    void $ forkServerAcidRemote st (UnixSocket _databaseSocket)

    return st

makeServerSt :: ServerConfig -> LoggerSet -> IO ServerSt
makeServerSt conf lgr = do
    ServerSt
        <$> openLocalState emptyDB
        <*> pure conf
        <*> newTVarIO mempty -- no-one is connected
        <*> newTVarIO mempty -- in lounge either
        <*> newBroadcastTChanIO
        <*> pure lgr
        <*> pure (error "No client")
        <*> newTVarIO mempty

restartGames :: Server ()
restartGames = do
    ss <- ask
    void $ query GetGames >>= imapM startGame >>= atomically . swapTVar (ss^.seWorkers)

startGame :: GID -> Game -> Server RunningGame
startGame gid game = do
    $logInfo $ "Starting game worker (" <> tshow gid <> ")"
    makeWorkerData game >>= forkWorker gid

-- * ACID Database

forkServerAcidRemote :: ServerSt -> PortID -> IO ThreadId
forkServerAcidRemote st port = flip forkFinally finalize $ withSocketsDo $ do
    sckt <- listenOn port
    acidServer' skipAuthenticationCheck sckt (st^.db)
  where
    finalize = \_ -> case port of
                   UnixSocket str -> removeFile str
                   _              -> return ()

openServerDB :: PortID -> IO (AcidState ServerDB)
openServerDB = openRemoteState skipAuthenticationPerform "127.0.0.1"

-- * Workers

-- | Game identifier
type GID = Int

-- | User identifier
type UID = Int

makeWorkerData :: Game -> Server WorkerData
makeWorkerData game = WorkerData
    <$> (liftIO . newTVarIO =<< attachClients game)
    <*> (liftIO $ newTVarIO $ NotBegun 5)
    <*> liftIO newEmptyTMVarIO
    <*> getWorkerLoggerSet game

getWorkerLoggerSet :: Game -> Server LoggerSet
getWorkerLoggerSet game = do
    logdir <- view $ serverConf.logDirectory.to unpack
    liftIO $ newFileLoggerSet defaultBufSize $ logdir </> "worker-" <> UUID.toString (_gameUUID game) <> ".log"

forkWorker :: GID -> WorkerData -> Server RunningGame
forkWorker gid wd = do
    chan     <- view seWatcher
    let die   = atomically . writeTChan chan . (gid,)
    threadId <- liftIO $ startWorker die wd
    return $ RunningGame wd threadId mempty

attachClients :: GameState UID -> Server (GameState Client)
attachClients gs = do
    cs <- rview seConnections
    return $ gs <&> \i -> fromMaybe (dummyClient "") (lookup i cs)

-- * Websocket

-- | Disconnecting websocket client.
data PartedException = PartedException Text deriving (Show, Typeable)
instance Exception PartedException

-- | On every new request, parse the first 'Event' and check whether it is
-- a 'JoinServer' event and the nick isn't taken. Invoke 'clientListener'
-- for the client on success, otherwise close the connection with an
-- appropriate 'Invalid' event.
wsApp :: ServerSt -> WS.ServerApp
wsApp st pending = do
    conn  <- WS.acceptRequest pending
    runClient (websocketClient "anonymous" conn) st initiateHandshake

initiateHandshake :: Server ()
initiateHandshake = do
    c <- view seClient
    event <- receive c
    case event of
        JoinServer nick ident token mgame
            | length nick > 24 -> uniError "Maximum nick length is 24 characters"
            | otherwise        -> withClient c{getNick = nick, getIdent = ident} (handshake token mgame)
        InternalControl secret -> internalConnect secret
        _                      -> uniError "Received an invalid Event"

handshake :: Text -> Maybe Int -> Server ()
handshake token mgame = do
    c <- view seClient
    time <- liftIO getCurrentTime

    $logInfo $ "New client " <> tshow (getIdent c) <> " (" <> getNick c <> ")"

    res <- update $ ConnectClient time (getIdent c) token
    case res of
        Left err -> uniError $ "Handshake failed: " <> err
        Right (i, cr)
            | Just g <- cr^.cInGame, Just g' <- mgame
            , g /= g'   -> uniError "You are already in another game"
            | otherwise -> let cr' | isJust mgame = cr&cInGame.~mgame
                                   | otherwise    = cr
                               c'                 = c { getIdent = i }
                               in withClient c' $ afterHandshake cr'

-- | After a successful handshake
afterHandshake :: ClientRecord -> Server ()
afterHandshake cr = do
    go <- ask <&> runServer
    liftIO $
        (go (connects cr) `catch` \case
            WS.CloseRequest _ _ -> throwIO (PartedException "Connection vanished")
            x                   -> throwIO (PartedException (tshow x))
        ) `catch` (go . disconnects)

-- ** Internal connect

-- | The WS listener of an internal connection.
internalConnect :: Text -> Server ()
internalConnect secret = do
    -- XXX: This could use handshaking before the websocket connection is
    -- accepted
    required <- view $ serverConf.websocketCtrlSecret
    if required /= secret
        then uni ("Error: wrong secret key" :: Text)
        else do c <- view seClient
                uni ("success" :: Text)
                forever $ receive c >>= \case
                    InternalNewGame settings -> createGame settings

-- * Worker Watcher

workerWatcher :: Server ()
workerWatcher = do
    chan <- view seWatcher >>= atomically . dupTChan
    forever $ atomically (readTChan chan) >>= uncurry workerDied

-- | Worker has finished: notify clients.
workerDied :: Int -> WorkerResult -> Server ()
workerDied gid res = do

    -- Log what happened
    case res of
        Left err -> $logError $ "Game " ++ tshow gid ++ " errored! " ++ tshow err
        Right x  -> $logInfo  $ "Game " ++ tshow gid ++ " finished. " ++ tshow x

    removedClients <- update $ DestroyGame gid -- TODO acid EventResult: should it be checked?
    $logInfo $ "Players " ++ tshow removedClients ++ " free'd"

    -- atomic
    ss <- ask
    (pg, clients) <- atomically $ do

        rg           <- readTVar (ss^.seWorkers) <&> (^?! at gid._Just)
        let wd        = rg^.gWorker
        gs           <- readTVar (wd^.wGame) <&> map getIdent
        machine      <- readTVar (wd^.wMachine)
        let clientSet = rg^..gClients.folded & setFromList
            pg        = PastGame (_Left %~ tshow $ res) gs machine

        modifyTVar' (ss^.seWorkers) (at gid .~ Nothing)
        modifyTVar' (ss^.seLounge) (union clientSet)

        return (pg, clientSet)

    -- Send part events
    forM_ clients $ \c -> safeUnicast c (PartGame (getNick c))

    -- Log the game
    update $ LogWorkerResult pg

------------------------------------------------------------------------------

-- * Clients

-- | Execute usual connection rituals with the new connection.
connects :: ClientRecord -> Server ()
connects cr = do
    c <- view seClient
    uni $ ClientIdentity (getNick c) (getIdent c) (cr^.cToken)

    ss <- ask
    atomically $ do modifyTVar' (ss^.seConnections) (at (getIdent c) .~ Just c)
                    modifyTVar' (ss^.seLounge) (insertSet c)
    updateLounge

    putLounge $ JoinServer (getNick c) (getIdent c) "" (cr^.cInGame)
    case cr^.cInGame of
        Nothing  -> return ()
        Just gid -> handleEventOf c (JoinGame gid (getNick c))
    talkClient

talkClient :: Server ()
talkClient = do
    c <- view seClient
    forever $ receive c >>= handleEventOf c

-- | Client disconnect rituals.
disconnects :: PartedException -> Server ()
disconnects (PartedException reason) = do
    c <- view seClient
    let i = getIdent c
    $logInfo $ "Client disconnected (" <> tshow i <> ")"

    cleanupClient reason

    ss <- ask
    Just cr <- query $ GetClientRecord i
    atomically $ do
        modifyTVar' (ss^.seConnections) (at i .~ Nothing)
        modifyTVar' (ss^.seLounge) (deleteSet c)
        case cr^.cInGame of
            Just gid -> modifyTVar' (ss^.seWorkers) (ix gid.gClients.at i .~ Nothing)
            Nothing  -> return ()
    updateLounge

    case cr^.cInGame of
        Just gid -> withRunningGame gid (`putWorker` WorkerPartPlayer c False (const $ return ()))
        Nothing  -> return ()

    putLounge (PartServer $ getNick c)

cleanupClient :: Text -> Server ()
cleanupClient reason = do
    c <- view seClient
    time <- liftIO getCurrentTime
    _ <- update $ PartClient time (c&getIdent) -- TODO EventResult: should this be checked?
    putLounge $ Message "" (getNick c <> " has left the server [" <> reason <> "]")

handleEventOf :: Client -> Event -> Server ()
handleEventOf c event = case event of
    JoinServer{}      -> uniError "Already joined (and nick change not implemented)"
    PartServer reason -> liftIO $ throwIO $ PartedException reason
    ClientIdentity{}  -> uniError "Unhandled event ClientIdentity"
    Message _ msg     -> do mgid <- query (GetClientRecord $ getIdent c) <&> (>>= _cInGame)
                            let m = Message (getNick c) msg
                            maybe (putLounge m) (flip withRunningGame $ mapM_ (flip safeUnicast m) . _gClients) mgid
    Invalid{}         -> uniError "Unhandled event Invalid"

    JoinGame n _      -> joinGame n
    PartGame _        -> partGame
    ForceStart gid    -> forceStart gid

    InGamePrivateEvent{} -> uniError "Unhandled event InGamePrivateEvent"
    InGameEvents{}    -> uniError "Unhandled event InGameEvents"
    InGameAction ga   -> handleGameAction ga

-- ** Actions

joinGame :: GID -> Server ()
joinGame gid = do
    c  <- view seClient
    $logInfo $ "Nick " <> getNick c <> " joins game " <> tshow gid

    Just cr <- query $ GetClientRecord (getIdent c)
    case cr^.cInGame of
        Just gid' | gid' /= gid -> uniError "You are already in some other game. You cannot join multiple games simultaneously."
        _                       -> withRunningGame gid handleJoinGame

  where

    handleJoinGame :: RunningGame -> Server ()
    handleJoinGame rg = do
        ss <- ask
        c  <- view seClient
        let nick = getNick c
            uid  = getIdent c
        putWorker rg $ WorkerAddPlayer c $ \gs -> do
            multicast gs (JoinGame gid nick)
            liftIO $ runServer ss $ do
                update $ SetPlayerGame gid uid
                update $ SetGame gid (getIdent <$> gs)
                atomically $ do modifyTVar' (ss^.seLounge) (deleteSet c)
                                modifyTVar' (ss^.seWorkers) (ix gid.gClients.at uid .~ Just c)
                putLounge (JoinGame gid nick)

partGame :: Server ()
partGame = withInGame $ \gid -> do
    st <- ask
    c  <- view seClient
    let uid  = getIdent c
        nick = getNick c
    $logInfo $ "Client " <> getNick c <> " leaving game " <> tshow gid
    withRunningGame gid $ \rg ->
        putWorker rg $ WorkerPartPlayer c True $ \gs -> do
            multicast gs (PartGame nick)
            liftIO $ runServer st $ do
                update $ AbandonPlayerGame uid gid (getIdent <$> gs)
                atomically $ do modifyTVar' (st^.seLounge) (insertSet c)
                                modifyTVar' (st^.seWorkers) (ix gid.gClients.at uid .~ Nothing)
                putLounge (PartGame nick)

-- | Force the specfied game to start even if there are not enough players.
forceStart :: Int -> Server ()
forceStart n = withRunningGame n (`putWorker` WorkerForceStart)

-- | Pass the TA to relevant worker
handleGameAction :: GameAction -> Server ()
handleGameAction action = withInGame $ \n -> withRunningGame n $ \gr -> do
    c <- view seClient
    putWorker gr (WorkerGameAction c action)

createGame :: GameSettings -> Server ()
createGame settings = do
    uuid <- liftIO randomIO
    let game = newEmptyGS 0 uuid settings
    res <- update (InsertGame game)
    case res of
        Left err -> uni $ InternalError err
        Right g  -> do rg <- startGame g game
                       ws <- view seWorkers
                       atomically $ modifyTVar ws (insertMap g rg)
                       uni $ InternalGameCreated g

-- * Utility

putWorker :: RunningGame -> WorkerInput -> Server ()
putWorker rg = atomically . putTMVar (rg^.gWorker.wInput)

putLounge :: Event -> Server ()
putLounge ev = rview seLounge >>= mapM_ (`safeUnicast` ev)

uni :: (Show e, WS.WebSocketsData e) => e -> Server ()
uni ev = view seClient >>= (`safeUnicast` ev)

uniError :: Text -> Server ()
uniError txt = view seClient >>= (`unicastError` txt)

multicast :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) => GameState Client -> Event -> m ()
multicast gs event = mapM_ (`safeUnicast` event) (gs^.gamePlayers^..each)

-- | Send updated lounge to everyone there
updateLounge :: Server ()
updateLounge = do
    lounge <- rview seLounge
    games  <- query GetGames
    nicks  <- rview seWorkers <&> map (\x -> setFromList $ x^..gClients.folded.to getNick)

    let gameInfos  = intersectionWithMap (\g ns -> (g^.gameSettings, ns, g^.gameUUID.to UUID.toText)) games nicks
        loungeInfo = LoungeInfo $ Lounge (mapMonotonic getNick lounge) gameInfos

    forM_ lounge (`safeUnicast` loungeInfo)

withInGame :: (GID -> Server ()) -> Server ()
withInGame f = do
    c        <- view seClient
    Just res <- query $ GetClientRecord (getIdent c)
    maybe (uniError "You are not in a game") f (res^.cInGame)

withRunningGame :: GID -> (RunningGame -> Server ()) -> Server ()
withRunningGame gid f = do
    res <- rview seWorkers <&> view (at gid)
    maybe (uniError $ "Game " <> tshow gid <> " not found!") f res

randomToken :: IO Text
randomToken = liftM pack $ replicateM 16 $ randomRIO ('A', 'Z')

------------------------------------------------------------------------------

-- * Debugger

-- | How to use the debugger:
--
--      d   dump server state
--      c   dump connected clients
--      g   dump ongoing games
--      l <id> <file> load game state from a file
--      s <id> <file> save game state to a file
serverDebugger :: Server ()
serverDebugger = forever $ do
    inp <- getLine <&> asText
    case inp of
        ""  -> return ()
        "d" -> print =<< query DumpDB
        "c" -> do print' "connections: " =<< rview seConnections
                  print' "lounge:      " =<< rview seLounge
        "g" -> mapM_ debugGameShow . itoList =<< rview seWorkers

        _ | ["l", gid', filename] <- words inp, Just gid <- readMay gid' -> do
                machine' : kyoku' <- readFile (unpack filename) <&> lines
                let Just machine = readMay machine'
                    kyoku        = read $ unpack (unlines kyoku' :: Text) -- TODO: readMay fails here, why?
                withRunningGame gid $ flip putWorker (WorkerReplaceKyoku machine kyoku)

          | ["s", gid', filename] <- words inp, Just gid <- readMay gid' -> withRunningGame gid $ \rg -> do
                let wd = _gWorker rg
                output <- liftIO $ (\m k -> show m ++ "\n" ++ show (_gameKyoku k)) <$> readTVarIO (_wMachine wd) <*> readTVarIO (_wGame wd)
                writeFile (unpack filename) output
          | otherwise -> putStrLn "Unknown command"
    putStrLn ""
  where

    print' :: Show a => Text -> a -> Server ()
    print' t x = putStrLn (t ++ tshow x)

    debugGameShow (n, g) = liftIO $ do
        putStrLn $ "Game: " ++ tshow n
        readTVarIO (g^.gWorker.wGame) >>= putDoc . pretty . fmap (unpack . getNick)
