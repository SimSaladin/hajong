{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
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
import           Network (PortID)
import           System.Log.FastLogger (LoggerSet, pushLogStr, toLogStr)
import           System.Random
import           Text.PrettyPrint.ANSI.Leijen (putDoc)

------------------------------------------------------------------------------

-- * Types

data Game = Game
    { _gaSettings      :: GameSettings
    , _gaState         :: GameState Int
    } deriving (Show, Typeable)

-- | This is ACID.
--
-- Players are identified with unique Ints. When joining the server, they
-- may opt for a new *anonymous* identifier or provide their previous
-- identifier and a passphrase.
data ServerDB = ServerDB
    { _sePlayerRecord  :: Record              -- ^ Identifiers
    , _sePlayerAuth    :: IntMap Text         -- ^ Auth tokens
    , _seInGame        :: IntMap Int          -- ^ In games, connected or not
    , _seGameRecord    :: Record              -- ^ Temp game id's
    , _seGames         :: IntMap Game
    } deriving (Show, Typeable)

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

-- * Transactions

isInGame :: Int -> Query ServerDB (Maybe Int)
isInGame i = view (seInGame.at i)

getGames :: Query ServerDB (IntMap Game)
getGames = view seGames

getGame :: Int -> Query ServerDB (Maybe Game)
getGame i = view (seGames.at i)

dumpDB :: Query ServerDB ServerDB
dumpDB = ask

clientToGame :: Int -> Int -> Update ServerDB ()
clientToGame gid i = seInGame.at i .= Just gid

destroyGame :: Int -> Update ServerDB ()
destroyGame gid = do seGameRecord %= freeId gid
                     cs <- preuse (seGames.at gid._Just.gaState.gamePlayers)
                     forM_ (maybe [] (^..each) cs) $ \i ->
                        seInGame.at i .= Nothing
                     seGames.at gid .= Nothing

insertGame :: Game -> Update ServerDB (Either Text Int)
insertGame game = do
    rec <- use seGameRecord
    case newId rec of
        Just (gid, rec') -> do sePlayerRecord .= rec'
                               seGames.at gid .= Just game
                               return (Right gid)
        Nothing -> return (Left "Server's Game capacity reached")

-- | Try re-connecting the client if auth token was supplied. If no auth
-- token is given record it as anonymous. Also fails if the nick was taken.
tryConnectClient :: Int -> Maybe Text -> Update ServerDB (Either Text (Int, Maybe Int))
tryConnectClient mi token = if' (mi > 0) tryReconnect connectAnon
  where
    tryReconnect = case token of
        Nothing -> return $ Left "ident given but no auth token"
        Just t  -> do 
            mt <- use (sePlayerAuth.at mi)
            if Just t == mt then connect mi else return $ Left "Auth token didn't match"

    connectAnon = do
        rec <- use sePlayerRecord
        case newId rec of
            Nothing        -> return $ Left "Server has reached player limit"
            Just (i, rec') -> sePlayerRecord .= rec' >> connect i

    connect i = Right . (i,) <$> use (seInGame.at i)

getToken :: Int -> Query ServerDB (Maybe Text)
getToken i = view (sePlayerAuth.at i)

setToken :: Int -> Text -> Update ServerDB ()
setToken i t = sePlayerAuth.at i .= Just t

$(makeAcidic ''ServerDB [ 'isInGame, 'clientToGame
                        , 'insertGame, 'destroyGame, 'tryConnectClient, 'getGame, 'getGames
                        , 'getToken, 'setToken
                        , 'dumpDB])

------------------------------------------------------------------------------

-- * Entry points

-- | Invoke the 'workerWatcher' and the websocket 'app'.
runServerMain :: ServerSt -> IO ()
runServerMain st = do
    void $ runServer st restartGames
    void . forkIO $ runServer st workerWatcher
    WS.runServer "0.0.0.0" 8001 $ wsApp st

runServerAcidRemote :: ServerSt -> PortID -> IO ()
runServerAcidRemote st port = acidServer skipAuthenticationCheck port (st^.db)

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
        JoinServer nick ident token
            | length nick > 24 -> uniError "Maximum nick length is 24 characters"
            | otherwise        -> withClient c{getNick = nick, getIdent = ident} (handshake token)
        _                      -> uniError "Received an invalid Event"

handshake :: Maybe Text -> Server ()
handshake token = do
    c <- view seClient
    $logInfo $ "New client " <> tshow (getIdent c) <> " (" <> getNick c <> ")"
    res <- update' $ TryConnectClient (getIdent c) token
    case res of
        Right (i, mgid) -> withClient c{getIdent=i} (afterHandshake mgid)
        Left err        -> uniError $ "Handshake failed: " <> err

-- | After a successful handshake
afterHandshake :: Maybe Int -> Server ()
afterHandshake mgid = do
    go <- ask <&> runServer
    liftIO $ (go (connects mgid) `finally` go disconnects) `catch` \case
        PartedException nick i reason -> go $
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
    c  <- view seClient
    res <- query' $ IsInGame (getIdent c)
    maybe (uniError "You are not in a game") f res

withRunningGame :: Int -> (RunningGame -> Server ()) -> Server ()
withRunningGame gid f = do
    res <- rview seWorkers <&> view (at gid)
    maybe (uniError "Game not found") f res

randomToken :: IO Text
randomToken = liftM pack $ replicateM 16 $ randomRIO ('A', 'Z')

-- * Listen logic

-- | Execute usual connection rituals with the new connection.
connects :: Maybe Int -> Server ()
connects mgid = do
    c     <- view seClient
    token <- getOrCreateToken (getIdent c)
    uni $ ClientIdentity (getNick c) (getIdent c) token

    ss    <- ask
    atomically $ do modifyTVar' (ss^.seConnections) (at (getIdent c) .~ Just c)
                    modifyTVar' (ss^.seLounge) (insertSet c)
    updateLounge

    putLounge (liftA3 JoinServer getNick getIdent (pure Nothing) c)
    case mgid of
        Nothing  -> return ()
        Just gid -> handleEventOf c (JoinGame gid (getNick c))
    talkClient
    
getOrCreateToken :: Int -> Server Text
getOrCreateToken i = query' (GetToken i) >>= maybe newToken return
  where newToken = do t <- liftIO randomToken
                      update' $ SetToken i t
                      return t

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
    mg <- query' $ IsInGame i
    atomically $ do
        modifyTVar' (ss^.seConnections) (at i .~ Nothing)
        modifyTVar' (ss^.seLounge) (deleteSet c)
        case mg of
            Just gid -> modifyTVar' (ss^.seWorkers) (ix gid.gClients.at i .~ Nothing)
            Nothing  -> return ()
    updateLounge

    case mg of
        Just gid -> withRunningGame gid
            (`putWorker` WorkerPartPlayer c (const $ return ()))
        Nothing  -> return ()
    putLounge (PartServer $ getNick c)

handleEventOf :: Client -> Event -> Server ()
handleEventOf c event = case event of
    JoinServer{}      -> uniError "Already joined (and nick change not implemented)"
    PartServer reason -> liftIO $ throwIO $ PartedException (getNick c) (getIdent c) reason
    Message _ msg     -> putLounge $ Message (getNick c) msg
    CreateGame name   -> createGame name
    JoinGame n _      -> joinGame n
    ForceStart n      -> forceStart n
    InGameAction a    -> handleGameAction a
    _                 -> uniError $ "Event not allowed or not implemented (" <> tshow event <> ")"

-- ** Actions

createGame :: Text -> Server ()
createGame name = do
    let settings  = GameSettings name
        gamestate = newEmptyGS (dummyClient "") name
        game      = Game settings (gamestate <&> getIdent)
    res <- update' $ InsertGame game
    case res of
        Right gid -> do
            c       <- view seClient
            ss      <- ask
            running <- createWorker game >>= forkWorker gid
            let running' = running & gClients .~ singletonMap (getIdent c) c

            atomically $ modifyTVar' (ss^.seWorkers) (at gid .~ Just running')
            putLounge $ GameCreated (gid, name, running^..gClients.folded <&> getNick)
            joinGame gid

        Left err -> uniError err

joinGame :: Int -> Server ()
joinGame gid = do
    c  <- view seClient
    $logInfo $ "Nick " <> getNick c <> " joins game " <> tshow gid

    res <- query' $ IsInGame (getIdent c)
    case res of
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
