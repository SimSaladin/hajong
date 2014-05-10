{-# LANGUAGE RankNTypes, TemplateHaskell, OverloadedStrings, NoImplicitPrelude, FlexibleContexts #-}

module CLI where

import Riichi

import           ClassyPrelude hiding (finally, handle, toLower)
import           Data.Set (mapMonotonic)
import           Control.Lens
import           Control.Applicative
import           Control.Concurrent (forkFinally, killThread, myThreadId, ThreadId, threadDelay)
import           Control.Monad.Trans.Reader
import           Control.Monad.Reader.Class
import           Data.List (delete, elemIndex)
import           Data.Char (isUpper, toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import           System.Random
import           System.Console.Haskeline
import qualified Network.WebSockets as WS

type Nick = Text

data Client = Client Nick WS.Connection
instance Eq Client where
    Client a _ == Client b _ = a == b

instance Ord Client where
    Client a _ <= Client b _ = a <= b

getNick :: Client -> Nick
getNick (Client nick _) = nick

getConn :: Client -> WS.Connection
getConn (Client _ conn) = conn

data ServerState = ServerState
                 { _serverConnections :: Map Nick WS.Connection
                 , _serverLounge :: Set Client
                 , _serverGames :: Map Int (Text, RiichiState, Set Client)
                 , _serverCounter :: Int

                 }

data Lounge = Lounge
            { _loungeNicksIdle :: Set Nick
            , _loungeGames :: Map Int (Text, Set Nick)
            } deriving (Show, Read)

makeLenses ''Lounge

data Event = JoinServer Text -- ^ Nick
           | PartServer Text
           | LoungeInfo Lounge
           | Message Text Text -- ^ from, content
           | CreateGame Text
           | NewGame (Int, Text, Set Nick) -- name, nicks
           | StartGame RiichiPlayer
           | JoinGame Int Text -- ^ Game lounge
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
    forM_ ((state ^. serverLounge) `union` (state ^. serverGames . each . _3))
        $ (`unicast` event) . getConn

multicast :: Int -> Event -> ServerState -> IO ()
multicast nth event state =
    case state ^. serverGames . at nth of
        Nothing              -> putStrLn "ERROR (multicast): game not found"
        Just (_, _, clients) -> forM_ clients $ (`unicast` event) . getConn

unicast :: MonadIO m => WS.Connection -> Event -> m ()
unicast conn = liftIO . WS.sendTextData conn

-- * Server

dumpState :: ServerState -> IO ()
dumpState state = print
    ( state ^. serverCounter
    , state ^. serverLounge & mapMonotonic getNick
    , state ^. serverGames & over (each._3) (mapMonotonic getNick)
    )

serverMain :: IO ()
serverMain = do
    state <- atomically $ newTVar newServerState
    WS.runServer "0.0.0.0" 9160 $ serverApp state

newServerState :: ServerState
newServerState = ServerState mempty mempty mempty 0

serverApp :: TVar ServerState -> WS.ServerApp
serverApp stateVar pending = do
    conn  <- WS.acceptRequest pending
    event <- WS.receiveData conn
    dumpState =<< readTVarIO stateVar
    case event of
        JoinServer nick -> let
            client  = Client nick conn
            connect = do
                accepted <- atomically $ do
                    s <- readTVar stateVar
                    if s ^. serverConnections.at nick.to isNothing
                        then let s' = s & over serverConnections (insertMap nick conn)
                                        . over serverLounge (insertSet client)
                                in writeTVar stateVar s' >> return (Just s')
                        else return Nothing -- nick taken
                case accepted of
                    Just s  -> do
                        unicast conn $ LoungeInfo $ buildLounge s
                        broadcast (JoinServer nick) s
                        talk stateVar client
                    Nothing -> unicast conn nickTaken

            disconnect = do
                putStrLn $ "Client disconnected (" <> nick <> ")"
                s <- atomically $ do
                    modifyTVar stateVar
                        $ over serverConnections     (deleteMap nick)
                        . over serverLounge          (deleteSet client)
                        . over (serverGames.each._3) (deleteSet client)
                    readTVar stateVar
                broadcast (PartServer nick) s

            in do
                putStrLn $ "New client " <> nick
                finally connect disconnect

        _ -> do
            putStrLn $ pack $ "Received non-event first: " <> show event
            unicast conn (Invalid "No JoinServer received. Please identify yourself.")
    where
       nickTaken = Invalid "Nick already in use"

buildLounge :: ServerState -> Lounge
buildLounge s = Lounge (s ^. serverLounge & mapMonotonic getNick) $
    s ^. serverGames <&> ((,) <$> view _1 <*> mapMonotonic getNick . view _3)

talk :: TVar ServerState -> Client -> IO ()
talk stateVar client@(Client nick conn) = forever $ do
    event <- WS.receiveData conn :: IO Event
    state <- readTVarIO stateVar
    case event of
        JoinServer _      -> unicast conn $ Invalid "Already joined"
        PartServer reason -> broadcast (Message "" $ "User " <> nick <> " has left [" <> reason <> "]") state
        Message _ msg     -> broadcast (Message nick msg) state
        CreateGame name   -> serverCreateGame client name stateVar
        JoinGame n _      -> addToGame n client stateVar
        _ -> do
            unicast conn (Invalid "Event not allowed or not implemented.")
            print $ "[ignored event] " <> show event

addToGame :: Int -> Client -> TVar ServerState -> IO ()
addToGame n client@(Client nick conn) stateVar = do
    joined <- atomically $ do
        state <- readTVar stateVar
        case state ^. serverGames.at n of
            Nothing              -> return (Left "Game not found")
            Just (_, _, clients) ->
                if client `member` clients
                    then return $ Left "Already in that game"
                    else do
                        let state' = state
                                & over (serverGames.at n.traversed._3) (insertSet client)
                                . over serverLounge (deleteSet client)
                        writeTVar stateVar state'
                        return $ Right state'

    either (unicast conn . Invalid) (broadcast (JoinGame n nick)) joined

serverCreateGame :: Client -> Text -> TVar ServerState -> IO ()
serverCreateGame client@(Client nick conn) name stateVar = do
    game <- newRiichiState

    state <- atomically $ do
        state <- readTVar stateVar
        let counter = state ^. serverCounter
            state'  = state
                & over serverGames (insertMap counter (name, game, singletonSet client))
                . over serverCounter (+1)
                . over serverLounge (deleteSet client)
        writeTVar stateVar state'
        return state'

    let counter = state ^. serverCounter - 1
    broadcast (NewGame (counter, name, singletonSet nick)) state
    unicast conn $ JoinGame counter nick

-- * Client

type Output a = (Applicative m, MonadIO m, MonadReader ClientState m) => m a

type UI = ReaderT ClientState (InputT IO)

data ClientState = ClientState
                 { _clientConn         :: WS.Connection
                 , _clientNick         :: Text
                 , _clientMainThread   :: ThreadId
                 , _clientLounge       :: MVar Lounge
                 , _clientGame         :: MVar (Maybe RiichiPlayer)
                 , _clientReceiveChan  :: TChan Text
                 , _clientCanInterrupt :: MVar Bool
                 , _clientWaiting      :: MVar Int
                 }
makeLenses ''ClientState

newClientState :: WS.Connection -> Text -> IO ClientState
newClientState conn nick = ClientState conn nick
    <$> myThreadId
    <*> newMVar (Lounge mempty mempty)
    <*> newMVar Nothing
    <*> newTChanIO
    <*> newMVar True
    <*> newMVar (-1)

clientMain :: Maybe Handle -> IO ()
clientMain = WS.runClient "localhost" 9160 "/" . clientApp

clientApp :: Maybe Handle -> WS.ClientApp ()
clientApp mhandle conn = do
    ident <- liftM pack $ replicateM 5 $ randomRIO ('a', 'z')
    state <- newClientState conn ident

    putStrLn "Connected to server. Type ? for a list of available commands."

    listenerDone <- newEmptyMVar 
    listenerThread <- forkFinally (runReaderT clientReceiver state) $
        \_ -> putMVar listenerDone ()

    unicast conn (JoinServer ident)

    runInputTBehavior (maybe defaultBehavior useFileHandle mhandle) defaultSettings (clientInputLoop state)

    -- cleanup
    WS.sendClose conn (PartServer "Bye")
    killThread listenerThread
    _ <- takeMVar listenerDone
    putStrLn "Exited cleanly"

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

rmodify l f = view l >>= liftIO . (`modifyMVar_` f)

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

out :: Text -> Output ()
out txt = queue (return [txt])

-- * Input

clientInputLoop :: ClientState -> InputT IO ()
clientInputLoop state = withInterrupt loop
    where
        nick = state ^. clientNick
        loop = handle interrupt $ do
            liftIO $ threadDelay 500000 -- FIXME this line in tests only

            inGame <- liftIO (readMVar $ state ^. clientGame)
            gameN <- liftIO (readMVar $ state ^. clientWaiting)
            minput <- getInputChar $ unpack $ nick <> ":"
                <> if gameN >= 0
                       then maybe "waiting" (const "playing") inGame <> "[" <> tshow gameN <> "]"
                       else "idle"
                <> " % "

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
    gameWait <- rview clientWaiting
    lift $ case mgame of
        Nothing -> outputStrLn $ "In lounge" <> if gameWait >= 0 then ", ready for game n. " <> show gameWait else ""
        Just _  -> outputStrLn $ "In game n." <> show gameWait

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
                             _  -> toServer $ CreateGame name'

joinGame :: UI ()
joinGame = printGames >> uiAskParam "join game: " go
    where
        go str = case readMay str of
            Nothing -> lift $ outputStrLn $ "NaN: " <> str
            Just n  -> toServer $ JoinGame n ""

rawCommand :: UI ()
rawCommand = uiAskParam "send command: " go
    where
        go str = case readMay str of
            Nothing -> lift $ outputStrLn "Command not recognized"
            Just cmd -> toServer cmd

-- * Receive

clientEventHandler :: Event -> ReaderT ClientState IO ()
clientEventHandler (JoinServer nick)   = nickJoined nick
clientEventHandler (PartServer nick)   = nickParted nick
clientEventHandler (LoungeInfo lounge) = rswap clientLounge lounge >> queue printLounge
clientEventHandler (StartGame gstate)  = rswap clientGame (Just gstate) >> startGame
clientEventHandler (JoinGame n nick)   = handleJoinGame n nick 
clientEventHandler (NewGame info)      = gameCreated info
clientEventHandler (Message sayer msg) = out $ "<" <> sayer <> "> " <> msg
clientEventHandler x = queue (return ["Unhandled event: " <> tshow x])

clientReceiver :: ReaderT ClientState IO ()
clientReceiver = do
    conn <- view clientConn
    forever $ liftIO (WS.receiveData conn) >>= clientEventHandler

nickJoined :: Text -> Output ()
nickJoined nick = do
    me <- view clientNick
    if me == nick
        then out "Succesfully joined server."
        else do
            rmodify clientLounge $ return . over loungeNicksIdle (insertSet nick)
            out $ nick <> " has joined."

nickParted :: Text -> Output ()
nickParted nick = do
    n <- view clientNick
    if n == nick
        then out "You have left the server"
        else do
            rmodify clientLounge $ return
                . over loungeNicksIdle (deleteSet nick)
                . over (loungeGames.each._2) (deleteSet nick)
            out $ nick <> " has parted."

gameCreated :: (Int, Text, Set Nick) -> Output ()
gameCreated (n, name, nicks) = do
    out $ "New game `" <> ppGame n (name, nicks)
    rmodify clientLounge $ return . over loungeGames (insertMap n (name, nicks))

handleJoinGame :: Int -> Text -> Output ()
handleJoinGame n nick = do
    rmodify clientLounge $ return
        . over (loungeGames.at n.traversed._2) (insertSet nick)
        . over loungeNicksIdle (deleteSet nick)

    me <- liftM (== nick) $ view clientNick
    if me
        then do _ <- rswap clientWaiting n
                out ("Ready to start game " <> tshow n)
        else do lounge <- rview clientLounge
                let count = length $ view (loungeGames.at n.traversed._2) lounge
                    countInfo
                        | count < 4 = " (" <> tshow (4 - count) <> " more needed.)" 
                        | otherwise = " Game starting!"
                out $ nick <> " joined game no. " <> tshow n <> countInfo

-- * Output

printUsers :: Output [Text] -- (MonadIO m, MonadReader ClientState m) => m ()
printUsers = do
    lounge <- view clientLounge >>= liftIO . readMVar
    return [ "Users idle: " <> ppNicks (lounge ^. loungeNicksIdle)]

printGames :: Output [Text]
printGames = do
    lounge <- rview clientLounge
    let games = lounge^.loungeGames & imap ppGame & toListOf each
        in return $ case games of
               []     -> ["No games"]
               (x:xs) -> "Games: " <> x : map ("       " <>) xs
    where

ppGame :: Int -> (Text, Set Nick) -> Text
ppGame n (name,nicks) = mconcat ["(", tshow n, ") ", name, " [", ppNicks nicks, "]"]

ppNicks :: Set Nick -> Text
ppNicks nicks = case setToList nicks of
            [] -> ""
            (x:xs) -> foldl' (\a b -> a <> ", " <> b) x xs

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
