{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import           Data.Acid
import           Data.Acid.Remote
import           Data.SafeCopy
import           Data.ReusableIdentifiers
import           Data.Set                   (mapMonotonic)
import qualified Network.WebSockets         as WS
import           Network
import           System.Log.FastLogger (LoggerSet, pushLogStr, toLogStr)
import           System.Directory (removeFile)
import           System.Random
import           Text.PrettyPrint.ANSI.Leijen (putDoc)

------------------------------------------------------------------------------

-- * Types

data Game = Game
    { _gaSettings      :: GameSettings
    , _gaState         :: GameState Int
    } deriving (Show, Typeable)

data ClientRecord = ClientRecord
    { _cNick           :: Text
    , _cToken          :: Text
    , _cRegistered     :: Bool
    , _cJoined         :: Maybe UTCTime
    , _cParted         :: Maybe UTCTime
    , _cInGame         :: Maybe Int
    } deriving (Show, Typeable)

-- | This is ACID.
--
-- Players are identified with unique Ints. When joining the server, they
-- may opt for a new *anonymous* identifier or provide their previous
-- identifier and a passphrase.
data ServerDB = ServerDB
    { _sePlayerRecord  :: Record              -- ^ Identifiers, connected or in game
    , _seReserved      :: IntMap ClientRecord -- ^ Reserved places and nicks, "valid until"
    , _seNicks         :: Map Nick Int
    , _seGameRecord    :: Record              -- ^ Temp game id's
    , _seGames         :: IntMap Game
    } deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''ClientRecord)
$(deriveSafeCopy 0 'base ''ServerDB)

-- | In Reader.
data ServerSt = ServerSt
    { _db              :: AcidState ServerDB
    , _seConnections   :: TVar (IntMap Client)      -- ^ Everyone connected
    , _seLounge        :: TVar (Set Client)         -- ^ Lounge
    , _seWatcher       :: TChan (Int, WorkerResult) -- ^ Worker watcher, **broadcast chan**.
    , _seLoggerSet     :: LoggerSet                 -- ^ Fed to new workers
    , _seClient        :: Client                    -- ^ When serving a single client
    , _seWorkers       :: TVar (IntMap RunningGame)
    } deriving (Typeable)

data RunningGame = RunningGame
    { _gWorker         :: WorkerData
    , _gThread         :: ThreadId
    , _gClients        :: IntMap Client
    } deriving (Typeable)

data PartedException = PartedException Nick Int Text deriving (Show, Typeable)
instance Exception PartedException

newtype Server a = Server
    { unServer :: ReaderT ServerSt IO a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadReader ServerSt )

-- * Lenses

--
makeLenses ''Game
makeLenses ''ClientRecord
makeLenses ''RunningGame
makeLenses ''ServerDB
makeLenses ''ServerSt

instance MonadLogger Server where
    monadLoggerLog loc src lvl msg = do
        lgr <- view seLoggerSet
        let out = defaultLogStr loc src lvl (toLogStr msg)
        liftIO $ pushLogStr lgr out

-- * Safecopy

$(deriveSafeCopy 0 'base ''GameSettings)
$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''Tile)
$(deriveSafeCopy 0 'base ''MentsuKind)
$(deriveSafeCopy 0 'base ''Honor)
$(deriveSafeCopy 0 'base ''Number)
$(deriveSafeCopy 0 'base ''Sangen)
$(deriveSafeCopy 0 'base ''TileKind)
$(deriveSafeCopy 0 'base ''Deal)
$(deriveSafeCopy 0 'base ''DealResults)
$(deriveSafeCopy 0 'base ''AbortiveDraw)
$(deriveSafeCopy 0 'base ''Hand)
$(deriveSafeCopy 0 'base ''Value)
$(deriveSafeCopy 0 'base ''Discard)
$(deriveSafeCopy 0 'base ''TurnAction)
$(deriveSafeCopy 0 'base ''Yaku)
$(deriveSafeCopy 0 'base ''Mentsu)
$(deriveSafeCopy 0 'base ''ShoutKind)
$(deriveSafeCopy 0 'base ''Kaze)
$(deriveSafeCopy 0 'base ''ValuedHand)
$(deriveSafeCopy 0 'base ''Shout)
$(deriveSafeCopy 0 'base ''HandPublic)
$(deriveSafeCopy 0 'base ''GameEvent)
$(deriveSafeCopy 0 'base ''Game)

instance SafeCopy (GameState Int) where
    putCopy (GameState a b c) = contain $ do safePut a; safePut b; safePut c
    getCopy = contain $ GameState <$> safeGet <*> safeGet <*> safeGet

-- * Query

getClientRecord :: Int -> Query ServerDB (Maybe ClientRecord)
getClientRecord i = view (seReserved.at i)

getGames :: Query ServerDB (IntMap Game)
getGames = view seGames

getGame :: Int -> Query ServerDB (Maybe Game)
getGame i = view (seGames.at i)

dumpDB :: Query ServerDB ServerDB
dumpDB = ask

-- * Updates

-- | Connect with (ident, token).
connectClient :: UTCTime -> Int -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
connectClient time mi token = use (seReserved.at mi) >>= \case
    Nothing                     -> return (Left "Unknown identity")
    Just c | token == c^.cToken -> do let c' = c&cJoined.~Just time
                                      seReserved.at mi <.= Just c'
                                      return (Right (mi, c'))
           | otherwise          -> return $ Left "Auth token didn't match"

partClient :: UTCTime -> Int -> Update ServerDB (Maybe ClientRecord)
partClient t i = use (seReserved.at i) >>= \case
        Just c  -> seReserved.at i <.= Just (c&cParted.~Just t)
        Nothing -> return Nothing

-- | New anonymous, new registered or previous registered.
newPlayerRecord :: Text -> Text -> Bool -> Update ServerDB (Either Text (Int, ClientRecord))
newPlayerRecord nick token reg = do
    taken <- use (seNicks.at nick)
    rec   <- use sePlayerRecord
    case (taken, newId rec) of
        -- nick is taken
        (Just i, _)         -> do Just c <- use (seReserved.at i)
                                  if | not reg        -> return (Left "Nick is taken")
                                     | c^.cRegistered -> return (Right (i, c))
                                     | otherwise      -> return (Left "Nick is used by someone anonymous") -- TODO overwrite his nick?
        -- server has room
        (_, Just (i, rec')) -> do sePlayerRecord  .= rec'
                                  seNicks.at nick .= Just i
                                  let c = ClientRecord nick token reg Nothing Nothing Nothing
                                  seReserved.at i .= Just c
                                  return (Right (i, c))
        -- server is full
        (_, Nothing)        -> return (Left "Server is full")

newAnon :: Text -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
newAnon nick token = newPlayerRecord nick token False

-- | Add or get info for registered user.
addRegisteredUser :: Text -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
addRegisteredUser user token = use (seNicks.at user) >>= \case
    Nothing -> newPlayerRecord user token True
    Just i  -> do
        Just c <- use (seReserved.at i)
        if c^.cRegistered then return (Right (i, c))
                          else return (Left "Your nick is in use by someone anonymous!")

clientToGame :: Int -> Int -> Update ServerDB ()
clientToGame gid i = seReserved.at i._Just.cInGame .= Just gid

destroyGame :: Int -> Update ServerDB [ClientRecord]
destroyGame gid = do seGameRecord %= freeId gid
                     cs <- preuse (seGames.at gid._Just.gaState.gamePlayers)
                     seGames.at gid .= Nothing
                     forM (maybe [] (^..each) cs) $ \i -> do
                        seReserved.at i._Just.cInGame .= Nothing
                        Just c <- use (seReserved.at i)
                        return c

insertGame :: Game -> Update ServerDB (Either Text Int)
insertGame game = do
    rec <- use seGameRecord
    case newId rec of
        Just (gid, rec') -> do sePlayerRecord .= rec'
                               seGames.at gid .= Just game
                               return (Right gid)
        Nothing -> return (Left "Server's Game capacity reached")

-- 
$(makeAcidic ''ServerDB [ 'getClientRecord, 'getGame, 'getGames, 'dumpDB
                        , 'connectClient, 'partClient, 'newAnon, 'addRegisteredUser
                        , 'clientToGame
                        , 'insertGame, 'destroyGame
                        ])

------------------------------------------------------------------------------

-- * Entry points

-- | Invoke the 'workerWatcher' and the websocket 'app'.
runServerMain :: ServerSt -> IO ()
runServerMain st = do
    void $ runServer st restartGames
    void . forkIO $ runServer st workerWatcher
    WS.runServer "0.0.0.0" 8001 $ wsApp st

-- | Returns the finalizer
forkServerAcidRemote :: ServerSt -> PortID -> IO (IO ())
forkServerAcidRemote st port = do
    _threadId <- forkIO $ withSocketsDo $ do
        sckt <- listenOn port
        acidServer' skipAuthenticationCheck sckt (st^.db)
    return $ case port of
                 UnixSocket str -> removeFile str
                 _ -> return ()

openServerDB :: PortID -> IO (AcidState ServerDB)
openServerDB = openRemoteState skipAuthenticationPerform "127.0.0.1"

-- * Initialize state

initServer :: LoggerSet -> IO ServerSt
initServer lgr = do
    sdb <- openLocalState emptyDB
    chan <- newBroadcastTChanIO
    ServerSt sdb
        <$> newTVarIO mempty -- no-one is connected
        <*> newTVarIO mempty -- in lounge either
        <*> pure chan
        <*> pure lgr
        <*> pure (error "No client")
        <*> newTVarIO mempty

emptyDB :: ServerDB
emptyDB = ServerDB (newRecord 1024) mempty mempty (newRecord 256) mempty

-- * Run Servers

runServer :: ServerSt -> Server a -> IO a
runServer st ma = runReaderT (unServer ma) st

runClient :: Client -> ServerSt -> Server a -> IO a
runClient client st = runServer (st&seClient.~client)

withClient :: Client -> Server a -> Server a
withClient c = local (seClient.~c)

-- * Games and workers

restartGames :: Server (IntMap RunningGame)
restartGames = do
    ss <- ask
    query' GetGames >>= imapM startGame >>= atomically . swapTVar (ss^.seWorkers)

startGame :: Int -> Game -> Server RunningGame
startGame gid game = do
    $logInfo $ "Starting game worker (" <> tshow gid <> ")"
    createWorker game >>= forkWorker gid

createWorker :: Game -> Server WorkerData
createWorker game = WorkerData (game^.gaSettings)
    <$> (liftIO . newTVarIO =<< attachClients (game^.gaState))
    <*> liftIO newEmptyTMVarIO
    <*> view seLoggerSet

attachClients :: GameState Int -> Server (GameState Client)
attachClients gs = do
    cs <- rview seConnections
    return $ gs <&> \i -> fromMaybe (dummyClient "") (lookup i cs)

forkWorker :: Int -> WorkerData -> Server RunningGame
forkWorker gid wd = do
    chan     <- view seWatcher
    let die   = atomically . writeTChan chan . (gid,)
    threadId <- liftIO $ startWorker die wd
    return $ RunningGame wd threadId mempty

-- * Websocket app

-- | On every new request, parse the first 'Event' and check whether it is
-- a 'JoinServer' event and the nick isn't taken. Invoke 'clientListener'
-- for the client on success, otherwise close the connection with an
-- appropriate 'Invalid' event.
wsApp :: ServerSt -> WS.ServerApp
wsApp st pending = do
    conn  <- WS.acceptRequest pending
    runClient (websocketClient "anonymous" conn) st initiateHandshake `catch` \case
        WS.CloseRequest _ _ -> return ()
        _                   -> return ()

initiateHandshake :: Server ()
initiateHandshake = do
    c <- view seClient
    event <- receive c
    case event of
        JoinServer nick ident token mgame
            | length nick > 24 -> uniError "Maximum nick length is 24 characters"
            | otherwise        -> withClient c{getNick = nick, getIdent = ident} (handshake token mgame)
        _                      -> uniError "Received an invalid Event"

handshake :: Text -> Maybe Int -> Server ()
handshake token mgame = do
    c <- view seClient
    $logInfo $ "New client " <> tshow (getIdent c) <> " (" <> getNick c <> ")"
    time <- liftIO getCurrentTime
    res <- update' $ ConnectClient time (getIdent c) token
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
    liftIO $ (go (connects cr) `finally` go disconnects) `catch` \case
        PartedException nick i reason -> go $ do
            time <- liftIO getCurrentTime
            update' $ PartClient time i
            putLounge $ Message "" (nick <> " has left the server [" <> reason <> "]")

-- * Worker Watcher

workerWatcher :: Server ()
workerWatcher = do
    chan <- view seWatcher >>= atomically . dupTChan
    forever $ atomically (readTChan chan) >>= uncurry workerDied

-- | Worker has finished; game ended.
workerDied :: Int -> WorkerResult -> Server ()
workerDied gid res = do
    case res of
        Left err -> $logError $ "Game " ++ tshow gid ++ " errored! " ++ tshow err
        Right x  -> $logInfo  $ "Game " ++ tshow gid ++ " finished. " ++ tshow x
    update' $ DestroyGame gid
    ss <- ask
    cs <- atomically $ do
        ws <- readTVar (ss^.seWorkers)
        let clientSet = ws ^?! at gid._Just.gClients ^.. folded & setFromList
        modifyTVar' (ss^.seWorkers) (at gid .~ Nothing)
        modifyTVar' (ss^.seLounge) (union clientSet)
        return clientSet
    forM_ cs $ \c -> unicast c (PartGame (getNick c))

------------------------------------------------------------------------------

-- * Utility

putWorker :: RunningGame -> WorkerInput -> Server ()
putWorker rg = atomically . putTMVar (rg^.gWorker.wInput)

putLounge :: Event -> Server ()
putLounge ev = rview seLounge >>= mapM_ (`unicast` ev)

uni :: Event -> Server ()
uni ev = view seClient >>= (`unicast` ev)

uniError :: Text -> Server ()
uniError txt = view seClient >>= (`unicastError` txt)

update' ev = view db >>= \d -> liftIO (update d ev)
query'  ev = view db >>= \d -> liftIO (query d ev)

-- | Send updated lounge to everyone there
updateLounge :: Server ()
updateLounge = do
    il <- rview seLounge
    lounge <- Lounge (il & mapMonotonic getNick) <$> (rview seWorkers <&> fmap f)
    forM_ il (`unicast` LoungeInfo lounge)
    where
        f x = (x^.gWorker.wSettings, x^.gClients^..folded & setFromList . map getNick)

withInGame :: (Int -> Server ()) -> Server ()
withInGame f = do
    c        <- view seClient
    Just res <- query' $ GetClientRecord (getIdent c)
    maybe (uniError "You are not in a game") f (res^.cInGame)

withRunningGame :: Int -> (RunningGame -> Server ()) -> Server ()
withRunningGame gid f = do
    res <- rview seWorkers <&> view (at gid)
    maybe (uniError "Game not found") f res

randomToken :: IO Text
randomToken = liftM pack $ replicateM 16 $ randomRIO ('A', 'Z')

-- * Listen logic

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
disconnects :: Server ()
disconnects = do
    c <- view seClient
    let i = getIdent c
    $logInfo $ "Client disconnected (" <> tshow i <> ")"

    ss <- ask
    Just cr <- query' $ GetClientRecord i
    atomically $ do
        modifyTVar' (ss^.seConnections) (at i .~ Nothing)
        modifyTVar' (ss^.seLounge) (deleteSet c)
        case cr^.cInGame of
            Just gid -> modifyTVar' (ss^.seWorkers) (ix gid.gClients.at i .~ Nothing)
            Nothing  -> return ()
    updateLounge

    case cr^.cInGame of
        Just gid -> withRunningGame gid
            (`putWorker` WorkerPartPlayer c (const $ return ()))
        Nothing  -> return ()
    putLounge (PartServer $ getNick c)

handleEventOf :: Client -> Event -> Server ()
handleEventOf c event = case event of
    JoinServer{}      -> uniError "Already joined (and nick change not implemented)"
    PartServer reason -> liftIO $ throwIO $ PartedException (getNick c) (getIdent c) reason
    Message _ msg     -> putLounge $ Message (getNick c) msg
    JoinGame n _      -> joinGame n
    ForceStart n      -> forceStart n
    InGameAction a    -> handleGameAction a
    _                 -> uniError $ "Event not allowed or not implemented (" <> tshow event <> ")"

-- ** Actions

joinGame :: Int -> Server ()
joinGame gid = do
    c  <- view seClient
    $logInfo $ "Nick " <> getNick c <> " joins game " <> tshow gid

    Just cr <- query' $ GetClientRecord (getIdent c)
    case cr^.cInGame of
        Just gid' | gid' /= gid -> uniError "You are already in some other game. You cannot join multiple games simultaneously."
        _                       -> withRunningGame gid (handleJoinGame gid)

handleJoinGame :: Int -> RunningGame -> Server ()
handleJoinGame gid rg = do
    ss <- ask
    c  <- view seClient
    let nick = getNick c
    putWorker rg $ WorkerAddPlayer c $ \gs -> do
        multicast gs (JoinGame gid nick)
        runServer ss $ do
            update' (ClientToGame gid (getIdent c))
            atomically $ modifyTVar' (ss^.seLounge) (deleteSet c)
            putLounge (JoinGame gid nick)

-- | Force the specfied game to start even if there are not enough players.
forceStart :: Int -> Server ()
forceStart n = withRunningGame n (`putWorker` WorkerForceStart)

-- | Pass the TA to relevant worker
handleGameAction :: GameAction -> Server ()
handleGameAction action = withInGame $ \n -> withRunningGame n $ \gr -> do
    c <- view seClient
    putWorker gr (c `WorkerGameAction` action)

------------------------------------------------------------------------------

-- * Debugger

serverDebugger :: Server ()
serverDebugger = forever $ do
    i <- getLine
    putStrLn ""
    case asText i of
        ""  -> return ()
        "d" -> print =<< query' DumpDB
        "c" -> do print' "connections: " =<< rview seConnections
                  print' "lounge:      " =<< rview seLounge
        "g" -> mapM_ debugGameShow . itoList =<< rview seWorkers
        _   -> putStrLn "Unknown command"
    putStrLn ""
  where
    print' :: Show a => Text -> a -> Server ()
    print' t x = putStrLn (t ++ tshow x)
    debugGameShow (n, g) = liftIO $ do
        putStrLn $ "Game: " ++ tshow n
        readTVarIO (g^.gWorker.wGame) >>= putDoc . pretty . fmap (unpack . getNick)
