{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, NoImplicitPrelude, FlexibleContexts #-}

module CLI where

import Riichi

import           ClassyPrelude hiding (finally, handle, toLower)
import           Control.Lens
import           Control.Applicative
import           Control.Concurrent (forkIO, killThread, myThreadId, ThreadId, threadDelay)
import           Control.Monad.Trans.Reader
import           Control.Monad.Reader.Class
import           Data.List (elemIndex)
import           Data.Char (isUpper, toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import           System.Random
import           System.Console.Haskeline
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

data ServerState = ServerState
                 { _serverGames :: Map Int (Text, RiichiState, [Client])
                 , _serverLounge :: Map Text WS.Connection
                 , _serverCounter :: Int
                 }

data Lounge = Lounge
            { _loungeNicksIdle :: [Text]
            , _loungeGames :: Map Int [Text] -- id -> nicks
            } deriving (Show, Read)

makeLenses ''Lounge

data Event = JoinServer Text -- ^ Nick
           | PartServer Text
           | LoungeInfo Lounge
           | Message Text Text -- ^ from, content
           | NewGame Text
           | JoinGame Int
           | StartGame RiichiPlayer
           | GameAction TurnAction
           | GameShout Shout
           | Invalid Text
           deriving (Show, Read)

instance WS.WebSocketsData Event where
    fromLazyByteString = fromMaybe (Invalid "Malformed event") . readMay . T.unpack . WS.fromLazyByteString
    toLazyByteString = WS.toLazyByteString . T.pack . show

makeLenses ''ServerState

broadcast :: Event -> ServerState -> IO ()
broadcast event state =
    forM_ ((state ^. serverLounge & toListOf each) ++ (state ^. serverGames & toListOf (each._3.each.to snd)))
        (`unicast` event)

multicast :: Int -> Event -> ServerState -> IO ()
multicast nth event state =
    case state ^. serverGames . at nth of
        Nothing              -> error "Game not found"
        Just (_, _, clients) -> forM_ clients $ \(_, conn) -> unicast conn event

unicast :: MonadIO m => WS.Connection -> Event -> m ()
unicast conn = liftIO . WS.sendTextData conn

-- * Server

dumpState :: ServerState -> IO ()
dumpState state = print
    ( state ^. serverCounter
    , state ^. serverLounge.to M.keys
    , state ^. serverGames & toListOf each & map (over _3 (map fst))
    )

serverMain :: IO ()
serverMain = do
    state <- atomically $ newTVar newServerState
    WS.runServer "0.0.0.0" 9160 $ serverApp state

newServerState :: ServerState
newServerState = ServerState mempty mempty 0

serverApp :: TVar ServerState -> WS.ServerApp
serverApp state pending = do
    conn  <- WS.acceptRequest pending
    event <- WS.receiveData conn
    -- atomically (readTVar state)
    -- dumpState =<<
    case event of
        JoinServer nick -> let

            connect = do
                accepted <- atomically $ do
                    s <- readTVar state
                    if s ^. serverLounge.at nick.to isJust
                        then return Nothing
                        else do
                            let s' = s & serverLounge %~ insertMap nick conn
                            writeTVar state s'
                            return (Just s')
                case accepted of
                    Nothing -> unicast conn $ Invalid "Nick already in use"
                    Just s  -> do
                        unicast conn $ LoungeInfo $ Lounge (s ^. serverLounge . to M.keys) mempty
                        broadcast (JoinServer nick) s
                        talk state (nick, conn)

            disconnect = do
                putStrLn "Client disconnected"
                s <- atomically $ do
                    modifyTVar state
                        $ over serverLounge (deleteMap nick)
                        . over (serverGames . each . _3) (deleteMap nick)
                    readTVar state
                broadcast (PartServer nick) s

            in do
                putStrLn $ "New client " <> nick
                finally connect disconnect

        _ -> do
            putStrLn $ pack $ "Received non-event first: " <> show event
            unicast conn (Invalid "No JoinServer received. Please identify yourself.")
    where

talk :: TVar ServerState -> Client -> IO ()
talk stateVar client@(user, conn) = forever $ do
    event <- WS.receiveData conn :: IO Event
    state <- readTVarIO stateVar
    case event of
        JoinServer _      -> unicast conn $ Invalid "Already joined"
        PartServer reason -> broadcast (Message "" $ "User " <> user <> " has left [" <> reason <> "]") state
        Message _ msg     -> broadcast (Message user msg) state
        NewGame name      -> serverCreateGame client name stateVar
        _ -> do
            unicast conn (Invalid "Event not allowed or not implemented.")
            print $ "[ignored event] " <> show event

serverCreateGame :: Client -> Text -> TVar ServerState -> IO ()
serverCreateGame client@(nick,conn) name stateVar = do
    game <- newRiichiState

    counter <- atomically $ do
        state <- readTVar stateVar
        let counter = state ^. serverCounter
        writeTVar stateVar $ state
            & over serverGames (insertMap counter (name, game, [client]))
            . over serverCounter (+1)
            . over serverLounge (deleteMap nick)
        return counter

    unicast conn $ JoinGame counter

-- * Client

type Output a = (Applicative m, MonadIO m, MonadReader ClientState m) => m a

type UI = ReaderT ClientState (InputT IO)

data ClientState = ClientState
                 { _clientConn :: WS.Connection
                 , _clientMainThread :: ThreadId
                 , _clientLounge :: MVar Lounge
                 , _clientGame  :: MVar (Maybe RiichiPlayer)
                 , _clientReceiveChan :: TChan Text
                 , _clientCanInterrupt :: MVar Bool
                 }
makeLenses ''ClientState

newClientState :: WS.Connection -> IO ClientState
newClientState conn = ClientState conn
    <$> myThreadId
    <*> newMVar (Lounge [] mempty)
    <*> newMVar Nothing
    <*> newTChanIO
    <*> newMVar True

clientMain :: Maybe Handle -> IO ()
clientMain = WS.runClient "localhost" 9160 "/" . clientApp

clientApp :: Maybe Handle -> WS.ClientApp ()
clientApp mhandle conn = do
    state <- newClientState conn
    ident <- liftM pack $ replicateM 5 $ randomRIO ('a', 'z')

    putStrLn "Connected to server. Type 'help' for available commands."

    listenerThread <- forkIO (runReaderT clientReceiver state)

    unicast conn (JoinServer ident)

    runInputTBehavior (maybe defaultBehavior useFileHandle mhandle) defaultSettings (clientInputLoop state)

    -- cleanup
    WS.sendClose conn (PartServer "Bye")
    putStrLn "Parted successfully"
    killThread listenerThread
    threadDelay 500000 -- FIXME wait for listener thread

-- ** Helpers

uiNoInterrupt :: UI a -> UI a
uiNoInterrupt f = do
    ivar <- view clientCanInterrupt
    (liftIO (swapMVar ivar False) >> f) `finally` liftIO (swapMVar ivar True)

uiAskParam :: String -> (String -> UI ()) -> UI ()
uiAskParam prompt f = lift (getInputLine prompt) >>= maybe (return ()) f

rview :: (MonadReader s m, MonadIO m) => Getting (MVar b) s (MVar b) -> m b
rview l = view l >>= liftIO . readMVar

rswap :: (MonadReader s m, MonadIO m) => Getting (MVar b) s (MVar b) -> b -> m b 
rswap l a = view l >>= liftIO . (`swapMVar` a)

toServer :: Event -> Output ()
toServer ev = view clientConn >>= (`unicast` ev)

ui :: UI [Text] -> UI ()
ui f = f >>= lift . outputStr . unpack . unlines

-- | Print to main thread's haskeline via a chan from any thread.
queue :: Output [Text] -> Output ()
queue f = do
    chan <- view clientReceiveChan
    interrupt <- rview clientCanInterrupt

    f >>= mapM_ (liftIO . atomically . writeTChan chan)

    when interrupt $ view clientMainThread >>= liftIO . (`throwTo` Interrupt)

-- * Input

clientInputLoop :: ClientState -> InputT IO ()
clientInputLoop state = withInterrupt loop
    where
        loop = handle interrupt $ do
            minput <- getInputChar "% "
            -- putStrLn $ pack $ show minput
            case minput of
                Nothing  -> loop
                Just 'q' -> return ()
                Just input -> runReaderT (clientInputHandler input) state >> loop

        interrupt Interrupt = do
            mline <- liftIO (atomically $ tryReadTChan $ state ^. clientReceiveChan)
            maybe loop (\line -> outputStrLn (unpack line) >> interrupt Interrupt) mline

clientInputHandler :: Char -> UI ()
clientInputHandler '?' = printHelp
clientInputHandler '!' = printStatus
clientInputHandler ' ' = uiNoInterrupt chatMessage
clientInputHandler  x  = do
    inGame <- rview clientGame
    if isJust inGame then gameHandler x else loungeHandler x
    where
        loungeHandler 'n' = ui printUsers
        loungeHandler 'g' = ui printGames
        loungeHandler 'c' = uiNoInterrupt createGame
        loungeHandler 'j' = uiNoInterrupt joinGame
        loungeHandler 'r' = uiNoInterrupt rawCommand
        loungeHandler  _  = unknownCommand

        gameHandler 'p' = undefined
        gameHandler 'c' = undefined
        gameHandler 'k' = undefined
        gameHandler 'r' = undefined
        gameHandler 'l' = undefined
        gameHandler  _  = case elemIndex (toLower x) discardKeys of
                              Nothing -> unknownCommand
                              Just n -> discardTile n (isUpper x)

        discardKeys = "aoeuidhtns-mwvz"

        unknownCommand = lift $ outputStrLn $ "Command `" <> [x] <> "` not recognized"

discardTile :: Int -> Bool -> UI ()
discardTile n riichi = do
    Just game <- rview clientGame
    let tiles = game ^. riichiHand . handConcealed ++ maybeToList (game ^. riichiHand . handPick)
    case tiles ^? traversed.index n of
        Nothing   -> lift $ outputStrLn $ "No tile at index " <> show n
        Just tile -> toServer $ GameAction $ Discard (_riichiPlayer game) tile riichi

printHelp :: UI ()
printHelp = lift $ outputStrLn $ unlines
    [ "Every command is initiated by it's first [l]etter"
    , ""
    , "Global commands"
    , "  [?] (help)                 Show this help text"
    , "  [!] (status)               Show status information"
    , "  <Space> (message)          Open a line prompt, send with enter."
    , "  [q]uit                     Close the client"
    , ""
    , "Messages are received in lounge or in-game only, corresponding to"
    , "your location. Use `l ` to send messages to lounge from a game"
    , ""
    , "In lounge"
    , "  [n]ames                    Show idle users"
    , "  [g]ames                    Show all games and players"
    , "  [c]reate <name>            Create a new game"
    , "  [j]oin <name>              Join game"
    , "  [r]aw <cmd>                Send direct protocol command (for debugging only)"
    , ""
    , "In game"
    , "  [aoeuidhtns-mwvz]          Discard [n]:th tile"
    , "  [p]on [c]hi [k]an [r]on    Shout a discard or declare kantsu"
    , "  [l]ounge                   Interpret next letter as a lounge commend"
    ]

printStatus :: UI ()
printStatus = do
    mgame <- rview clientGame
    lift $ case mgame of
        Nothing -> outputStrLn "In lounge"
        Just game -> outputStrLn "In game"

chatMessage :: UI ()
chatMessage = lift (getInputLine "say: ") >>= send
    where
        send Nothing = return ()
        send (Just msg)
            | null msg  = return ()
            | otherwise = toServer $ Message "" (pack msg)

createGame :: UI ()
createGame = lift (getInputLine "Game name: ") >>= maybe (return ()) go
    where
        go name = let name' = T.dropWhileEnd (== ' ') $ T.dropWhile (== ' ') $ pack name
                      in case name' of
                             "" -> lift (outputStrLn "Name cannot be empty")
                             _  -> toServer $ NewGame name'

joinGame :: UI ()
joinGame = printGames >> uiAskParam "join game: " go
    where
        go str = case readMay str of
            Nothing -> lift $ outputStrLn $ "NaN: " <> str
            Just n  -> toServer $ JoinGame n

rawCommand :: UI ()
rawCommand = uiAskParam "send command: " go
    where
        go str = case readMay str of
            Nothing -> lift $ outputStrLn "Command not recognized"
            Just cmd -> toServer cmd

-- * Output

clientReceiver :: ReaderT ClientState IO ()
clientReceiver = do
    conn <- view clientConn
    forever $ liftIO (WS.receiveData conn) >>= clientEventHandler

clientEventHandler :: Event -> ReaderT ClientState IO ()
clientEventHandler (LoungeInfo lounge) = rswap clientLounge lounge >> queue printLounge
clientEventHandler (StartGame gstate)  = rswap clientGame (Just gstate) >> startGame
clientEventHandler x = queue (return ["Unhandled event: " <> tshow x])

printUsers :: Output [Text] -- (MonadIO m, MonadReader ClientState m) => m ()
printUsers = do
    lounge <- view clientLounge >>= liftIO . readMVar
    return [ "Users idle: " <> intercalate ", " (lounge ^. loungeNicksIdle)]

printGames :: Output [Text]
printGames = do
    lounge <- rview clientLounge
    return [ "Games: " <> mconcat (lounge ^. loungeGames ^.. each . to (intercalate ", ")) ]

printLounge :: Output [Text]
printLounge = liftA2 (++) printUsers printGames

startGame :: Output ()
startGame = 
    queue $ return ["Entering game!"]

-- * Main

hajongCLI :: IO ()
hajongCLI = do
    args <- getArgs
    case args of 
        (x:_) | "s" `isPrefixOf` x -> server
              | "c" `isPrefixOf` x -> client
        _ -> do
            putStrLn "Hajong v0. Type s for server, c for client."
            inp <- T.getLine
            case inp of
                "s" -> server
                _   -> client
    where
        server = putStrLn "Starting server..." >> serverMain
        client = putStrLn "Starting client..." >> clientMain Nothing
