{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- SafeCopy
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
--
-- This module defines ACID game server state,
-- and WebSocket input layer.
------------------------------------------------------------------------------
module Hajong.Server where

------------------------------------------------------------------------------
import           Hajong.Connections
import           Hajong.Client
import           Hajong.Worker
import           Mahjong
------------------------------------------------------------------------------
import           Prelude (read)
import           Control.Monad.Logger
import           Control.Concurrent
import           Data.Acid
import           Data.Acid.Remote
import           Data.SafeCopy
import           Data.ReusableIdentifiers
import           Data.Set                   (mapMonotonic)
import qualified Network.WebSockets         as WS
import           Data.Time.Clock (secondsToDiffTime)
import           Network
import           System.Log.FastLogger (LoggerSet, pushLogStr)
import           System.Directory (removeFile)
import           System.Random
import           Text.PrettyPrint.ANSI.Leijen (putDoc)
------------------------------------------------------------------------------

-- * Types

-- | Serialization of an on-going game.
data Game = Game
    { _gaSettings      :: GameSettings
    , _gaState         :: GameState Int
    } deriving (Show, Typeable)

-- | A record of a client connected or previously connected.
data ClientRecord = ClientRecord
    { _cNick           :: Text
    , _cToken          :: Text
    , _cRegistered     :: Bool
    , _cStatus         :: Either UTCTime UTCTime -- ^ Left (disconnected at) or Right (connected at)
    , _cInGame         :: Maybe Int
    } deriving (Show, Typeable)

-- | This is the root ACID type.
--
-- Players are identified with unique Ints. When joining the server, they
-- may opt for a new *anonymous* identifier or provide their previous
-- identifier and a passphrase.
data ServerDB = ServerDB
    { _sePlayerRecord  :: Record              -- ^ Player identifiers
    , _seReserved      :: IntMap ClientRecord -- ^ Clients identified by @sePlayerRecord@
    , _seNicks         :: Map Nick Int        -- ^ Reserved nicks
    , _seGameRecord    :: Record              -- ^ Game identifiers
    , _seGames         :: IntMap Game         -- ^ Games identified by @seGameRecord@
    } deriving (Show, Typeable)

$(deriveSafeCopy 1 'base ''ClientRecord)
$(deriveSafeCopy 0 'base ''ServerDB)

-- | Server logic Reader value.
data ServerSt = ServerSt
    { _db              :: AcidState ServerDB
    , _seConnections   :: TVar (IntMap Client)      -- ^ Everyone connected
    , _seLounge        :: TVar (Set Client)         -- ^ Lounge
    , _seWatcher       :: TChan (Int, WorkerResult) -- ^ Worker watcher, **broadcast chan**.
    , _seLoggerSet     :: LoggerSet                 -- ^ Fed to new workers
    , _seClient        :: Client                    -- ^ When serving a single client
    , _seWorkers       :: TVar (IntMap RunningGame)
    } deriving (Typeable)

-- | Game worker thread.
data RunningGame = RunningGame
    { _gWorker         :: WorkerData
    , _gThread         :: ThreadId
    , _gClients        :: IntMap Client
    } deriving (Typeable)

-- | Disconnecting websocket client.
data PartedException = PartedException Text deriving (Show, Typeable)
instance Exception PartedException

-- | Server logic monad.
newtype Server a = Server { unServer :: ReaderT ServerSt IO a }
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

$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''GameSettings)
$(deriveSafeCopy 0 'base ''Tile)
$(deriveSafeCopy 0 'base ''MentsuKind)
$(deriveSafeCopy 0 'base ''Honor)
$(deriveSafeCopy 0 'base ''Number)
$(deriveSafeCopy 0 'base ''Sangen)
$(deriveSafeCopy 0 'base ''TileKind)
$(deriveSafeCopy 0 'base ''KyokuResults)
$(deriveSafeCopy 0 'base ''AbortiveDraw)
$(deriveSafeCopy 0 'base ''Flag)
$(deriveSafeCopy 0 'base ''Value)
$(deriveSafeCopy 0 'base ''Discard)
$(deriveSafeCopy 0 'base ''TurnAction)
$(deriveSafeCopy 0 'base ''Yaku)
$(deriveSafeCopy 0 'base ''Mentsu)
$(deriveSafeCopy 0 'base ''ShoutKind)
$(deriveSafeCopy 0 'base ''Kaze)
$(deriveSafeCopy 0 'base ''ValuedHand)
$(deriveSafeCopy 0 'base ''Shout)
$(deriveSafeCopy 0 'base ''GameEvent)
$(deriveSafeCopy 0 'base ''Game)
$(deriveSafeCopy 0 'base ''RiichiState)
$(deriveSafeCopy 0 'base ''DrawState)
$(deriveSafeCopy 0 'base ''FuritenState)
$(deriveSafeCopy 0 'base ''HandFlag)

-- SafeCopy instances for indexed types

instance (SafeCopy (m (Set HandFlag)), SafeCopy (m Bool), SafeCopy (m [Tile]), SafeCopy (m FuritenState), SafeCopy (PickedTile m)) => SafeCopy (Hand m) where
    version          = 0
    putCopy Hand{..} = contain $ do safePut _handCalled; safePut _handDiscards; safePut _handRiichi; safePut _handIppatsu; safePut _handState; safePut _handPicks; safePut _handConcealed; safePut _handFuriten; safePut _handCanTsumo; safePut _handFlags
    getCopy          = contain $ Hand <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy (m Tile) => SafeCopy (PickedTile m) where
    version = 0
    putCopy (FromWall t) = contain $ do safePut (0 :: Word8); safePut t
    putCopy (FromWanpai t) = contain $ do safePut (1 :: Word8); safePut t
    putCopy (AgariTsumo t) = contain $ do safePut (2 :: Word8); safePut t
    putCopy (AgariCall t k) = contain $ do safePut (3 :: Word8); safePut t; safePut k
    putCopy (AgariChankan t k) = contain $ do safePut (4 :: Word8); safePut t; safePut k
    putCopy (AgariTsumoWanpai t) = contain $ do safePut (5 :: Word8); safePut t
    getCopy = contain $ do tag <- safeGet
                           case tag :: Word8 of
                               0 -> FromWall <$> safeGet
                               1 -> FromWanpai <$> safeGet
                               2 -> AgariTsumo <$> safeGet
                               3 -> AgariCall <$> safeGet <*> safeGet
                               4 -> AgariChankan <$> safeGet <*> safeGet
                               5 -> AgariTsumoWanpai <$> safeGet
                               _ -> fail $ "Couldn't identify tag " ++ show tag

instance SafeCopy a => SafeCopy (Identity a) where
    putCopy (Identity a) = contain $ safePut a
    getCopy = contain $ Identity <$> safeGet

instance SafeCopy (Hand m) => SafeCopy (Kyoku' m) where
    version = 0
    putCopy Kyoku{..} = contain $ do safePut _pRound; safePut _pTurn; safePut _pFlags; safePut _pOja; safePut _pFirstOja; safePut _pWallTilesLeft; safePut _pDora; safePut _pPlayers; safePut _pHonba; safePut _pRiichi; safePut _pResults; safePut _pDeals; safePut _sEvents; safePut _sHands; safePut _sWall; safePut _sWanpai; safePut _sWaiting;
    getCopy = contain $ do Kyoku <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet 

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

-- | Add the new client to ServerDB if
--      * its @ident@ is known,
--      * the client knows correct @token@ and
--      * there are no other clients with the @ident@
--
--  @connectClient currentTime ident token@
connectClient :: UTCTime -> Int -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
connectClient time ident token = do
    reserved <- use (seReserved.at ident)
    case reserved of
        Nothing                     -> return $ Left $ "Unknown identity: " <> tshow ident
        Just c | token == c^.cToken -> do let c' = c&cStatus.~Right time
                                          _ <- seReserved.at ident <.= Just c' -- TODO EventResult: should it be checked?
                                          return (Right (ident, c'))
               | otherwise          -> return $ Left $ "Auth tokens didn't match (got " <> token <> ")"

-- | Client disconnects: Set status to Left in corresponding ClientRecord. 
--
-- @partClient currentTime ident@
partClient :: UTCTime -> Int -> Update ServerDB (Maybe ClientRecord)
partClient time ident = use (seReserved.at ident) >>= \case
        Just c  -> seReserved.at ident <.= Just (c&cStatus.~Left time)
        Nothing -> return Nothing

-- | Assign a @ident@ to a new player if possible.
--
-- @registerPlayer nick token isRegistered@
registerPlayer :: Text -> Text -> Bool -> Update ServerDB (Either Text (Int, ClientRecord))
registerPlayer nick token reg = do
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
                                  let c = ClientRecord nick token reg (Left $ UTCTime (ModifiedJulianDay 0) $ secondsToDiffTime 0) Nothing
                                  seReserved.at i .= Just c
                                  return (Right (i, c))
        -- server is full
        (_, Nothing)        -> return (Left "Server is full")

-- | Assign an @ident@ to an anonymous player.
--
-- @registerAnonymousPlayer nick token@
registerAnonymousPlayer :: Text -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
registerAnonymousPlayer nick token = registerPlayer nick token False

-- | Assign an @ident@ to a registered player.
--
-- *TODO: If the nick is taken by an anon player, we should force the anon to change his nick.*
--
-- @registerLoggedInPlayer nick token@
registerLoggedInPlayer :: Text -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
registerLoggedInPlayer nick token = use (seNicks.at nick) >>= \case
    Nothing -> registerPlayer nick token True
    Just i  -> do
        Just c <- use (seReserved.at i)
        if c^.cRegistered then return (Right (i, c))
                          else return (Left "Your nick is in use by someone anonymous!")

-- | Set the game of a player
setPlayerGame :: Int -> Int -> Update ServerDB ()
setPlayerGame gid i = seReserved.at i._Just.cInGame .= Just gid

destroyGame :: Int -> Update ServerDB [ClientRecord]
destroyGame gid = do seGameRecord %= freeId gid
                     cs <- preuse (seGames.at gid._Just.gaState.gamePlayers)
                     seGames.at gid .= Nothing
                     rs <- forM (maybe [] (^..each) cs) $ \i -> do
                        seReserved.at i._Just.cInGame .= Nothing
                        use (seReserved.at i)
                     return (catMaybes rs)

insertGame :: Game -> Update ServerDB (Either Text Int)
insertGame game = do
    rec <- use seGameRecord
    case newId rec of
        Just (gid, rec') -> do sePlayerRecord .= rec'
                               seGames.at gid .= Just game
                               return (Right gid)
        Nothing -> return (Left "Server's Game capacity reached")

-- * ACID interface
$(makeAcidic ''ServerDB [ 'getClientRecord, 'getGame, 'getGames, 'dumpDB
                        , 'connectClient, 'partClient
                        , 'registerAnonymousPlayer, 'registerLoggedInPlayer
                        , 'setPlayerGame
                        , 'insertGame, 'destroyGame
                        ])

------------------------------------------------------------------------------

-- * Entry points

-- | Invoke the 'workerWatcher' and the websocket 'app'.
runServerMain :: ServerSt -> IO ()
runServerMain st = do
    runServer st restartGames
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

restartGames :: Server ()
restartGames = do
    ss <- ask
    void $ query' GetGames >>= imapM startGame >>= atomically . swapTVar (ss^.seWorkers)

startGame :: Int -> Game -> Server RunningGame
startGame gid game = do
    $logInfo $ "Starting game worker (" <> tshow gid <> ")"
    createWorker game >>= forkWorker gid

createWorker :: Game -> Server WorkerData
createWorker game = WorkerData (game^.gaSettings)
    <$> (liftIO . newTVarIO =<< attachClients (game^.gaState))
    <*> (liftIO $ newTVarIO $ NotBegun 5)
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
    liftIO $
        (go (connects cr) `catch` \case
            WS.CloseRequest _ _ -> throwIO (PartedException "Connection vanished")
            x                   -> throwIO (PartedException (tshow x))
        ) `catch` (go . disconnects)

-- * Worker Watcher

workerWatcher :: Server ()
workerWatcher = do
    chan <- view seWatcher >>= atomically . dupTChan
    forever $ atomically (readTChan chan) >>= uncurry workerDied

-- | Worker has finished: notify clients.
workerDied :: Int -> WorkerResult -> Server ()
workerDied gid res = do

    -- Log and then destroy the game
    case res of
        Left err -> $logError $ "Game " ++ tshow gid ++ " errored! " ++ tshow err
        Right x  -> $logInfo  $ "Game " ++ tshow gid ++ " finished. " ++ tshow x
    _ <- update' $ DestroyGame gid -- TODO EventResult: should it be checked?

    ss <- ask
    clients <- atomically $ do
        ws <- readTVar (ss^.seWorkers)
        let clientSet = ws ^?! at gid._Just.gClients ^.. folded & setFromList
        modifyTVar' (ss^.seWorkers) (at gid .~ Nothing)
        modifyTVar' (ss^.seLounge) (union clientSet)
        return clientSet
    forM_ clients $ \c -> unicast c (PartGame (getNick c))

------------------------------------------------------------------------------

-- * Utility

putWorker :: RunningGame -> WorkerInput -> Server ()
putWorker rg = atomically . putTMVar (rg^.gWorker.wInput)

putLounge :: Event -> Server ()
putLounge ev = rview seLounge >>= mapM_ (`unicast` ev)

uni :: WS.WebSocketsData e => e -> Server ()
uni ev = view seClient >>= (`unicast` ev)

uniError :: Text -> Server ()
uniError txt = view seClient >>= (`unicastError` txt)

-- Writing typesigs for these helpers would require types from the
-- acid-state package that are not exposed in the API.
update' ev = view db >>= \d -> liftIO (update d ev)
query'  ev = view db >>= \d -> liftIO (query d ev)

multicast :: MonadIO m => GameState Client -> Event -> m ()
multicast gs event = mapM_ (`unicast` event) (gs^.gamePlayers^..each)

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
    maybe (uniError $ "Game " <> tshow gid <> " not found!") f res

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
disconnects :: PartedException -> Server ()
disconnects (PartedException reason) = do
    c <- view seClient
    let i = getIdent c
    $logInfo $ "Client disconnected (" <> tshow i <> ")"

    cleanupClient reason

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

cleanupClient :: Text -> Server ()
cleanupClient reason = do
    c <- view seClient
    time <- liftIO getCurrentTime
    _ <- update' $ PartClient time (c&getIdent) -- TODO EventResult: should this be checked?
    putLounge $ Message "" (getNick c <> " has left the server [" <> reason <> "]")

handleEventOf :: Client -> Event -> Server ()
handleEventOf c event = case event of
    JoinServer{}      -> uniError "Already joined (and nick change not implemented)"
    PartServer reason -> liftIO $ throwIO $ PartedException reason
    Message _ msg     -> putLounge $ Message (getNick c) msg -- TODO this should go to current game or lounge if in no game
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
            update' (SetPlayerGame gid (getIdent c))
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

createGame :: GameSettings -> Server ()
createGame settings = do
    let game = Game settings (newEmptyGS 0 "") -- TODO makes no sense
    res <- update' (InsertGame game)
    case res of
        Left err -> uni $ InternalError err
        Right g  -> do rg <- startGame g game
                       ws <- view seWorkers
                       atomically $ modifyTVar ws (insertMap g rg)
                       uni $ InternalGameCreated g 


-- * Internal

internalConnect :: Text -> Server ()
internalConnect _secret = do
    -- TODO Check secret
    c <- view seClient
    forever $ receive c >>= \case
        InternalNewGame settings -> createGame settings

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
        "d" -> print =<< query' DumpDB
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
                output <- liftIO $ (\m k -> show m ++ "\n" ++ show (_gameDeal k)) <$> readTVarIO (_wMachine wd) <*> readTVarIO (_wGame wd)
                writeFile (unpack filename) output
          | otherwise -> putStrLn "Unknown command"
    putStrLn ""
  where

    print' :: Show a => Text -> a -> Server ()
    print' t x = putStrLn (t ++ tshow x)

    debugGameShow (n, g) = liftIO $ do
        putStrLn $ "Game: " ++ tshow n
        readTVarIO (g^.gWorker.wGame) >>= putDoc . pretty . fmap (unpack . getNick)
