{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoImplicitPrelude, FlexibleContexts #-}

module CLI where

import Riichi

import ClassyPrelude hiding (finally, handle)
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent (forkIO, killThread, myThreadId, ThreadId)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Reader.Class
import System.Console.Haskeline
import System.Exit
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

data ServerState = ServerState
                 { _serverGames :: Map Int (RiichiState, [Client])
                 , _serverLounge :: [Client]
                 }

data Lounge = Lounge
            { _loungeNicksIdle :: [Text]
            , _loungeGames :: Map Int [Text] -- id -> nicks
            } deriving (Show, Read)

data Event = JoinServer Text -- ^ Nick
           | PartServer Text
           | GameAction TurnAction
           | GameShout Shout
           | LoungeInfo Lounge
           | NewClient Text
           | StartGame RiichiState
           | Message Text Text -- ^ from, content
           | Invalid
           deriving (Show, Read)

instance WS.WebSocketsData Event where
    fromLazyByteString = fromMaybe Invalid . readMay . T.unpack . WS.fromLazyByteString
    toLazyByteString = WS.toLazyByteString . T.pack . show


data ClientState = ClientState
                 { _clientConn :: WS.Connection
                 , _clientLounge :: MVar Lounge
                 , _clientGame  :: MVar (Maybe RiichiState)
                 , _clientReceiveChan :: TChan String
                 , _clientMainThread :: ThreadId
                 }

makeLenses ''ServerState
makeLenses ''ClientState
makeLenses ''Lounge

newServerState :: ServerState
newServerState = ServerState mempty []

broadcast :: Event -> ServerState -> IO ()
broadcast event state =
    forM_ ( (state ^. serverLounge) ++ (state ^. serverGames . each . _2) )
        (\(_, conn) -> unicast conn event)

multicast :: Int -> Event -> ServerState -> IO ()
multicast nth event state =
    case state ^. serverGames . at nth of
        Nothing           -> error "Game not found"
        Just (_, clients) -> forM_ clients $ \(_, conn) -> unicast conn event

unicast :: MonadIO m => WS.Connection -> Event -> m ()
unicast conn = liftIO . WS.sendTextData conn

serverApp :: MVar ServerState -> WS.ServerApp
serverApp state pending = do
    conn  <- WS.acceptRequest pending
    event <- WS.receiveData conn
    putStrLn "New client"
    case event of
        JoinServer nick -> flip finally disconnect $ do
            let client = (nick, conn)
            modifyMVar_ state $ \s -> if s ^. serverLounge . to (elem nick . map fst)
                then unicast conn Invalid >> return s
                else do
                    let s' = s & serverLounge %~ (client :)
                    unicast conn (LoungeInfo $ Lounge (s' ^. serverLounge . to (map fst)) mempty)
                    broadcast (NewClient nick) s'
                    return s'
            talk state client
        _ -> unicast conn Invalid
    where
        disconnect = do
            putStrLn "Disconnecting client"
            error "disconnect not ipmlemented"

talk :: MVar ServerState -> Client -> IO ()
talk state (user, conn) = forever $ do
    event <- WS.receiveData conn :: IO Event
    print event

printUsers :: (MonadIO m, MonadReader ClientState m) => m ()
printUsers = do
    lounge <- view clientLounge >>= liftIO . readMVar
    liftIO $ putStrLn $ "Users idle: " <> intercalate ", " (lounge ^. loungeNicksIdle)

printLounge :: (MonadIO m, MonadReader ClientState m) => m ()
printLounge = do
    printUsers
    putStrLn $ "Current games: "

startGame :: (MonadIO m, MonadReader ClientState m) => m ()
startGame = 
    liftIO $ putStrLn "Game!"

clientReceiver :: (MonadIO m, MonadReader ClientState m) => m ()
clientReceiver = do
    conn <- view clientConn
    forever $ liftIO (WS.receiveData conn) >>= clientEventHandler

-- | Print to main thread's haskeline via a chan from any thread.
clientPrint :: (MonadIO m, MonadReader ClientState m) => [String] -> m ()
clientPrint xs = do
    chan <- view clientReceiveChan
    mapM_ (liftIO . atomically . writeTChan chan) xs
    view clientMainThread >>= liftIO . (`throwTo` Interrupt)

clientInputLoop :: ClientState -> InputT IO ()
clientInputLoop state = withInterrupt loop
    where
        loop = handle interrupt $ runReaderT clientInputHandler state >> loop

        interrupt Interrupt = do
            mline <- liftIO (atomically $ tryReadTChan $ state ^. clientReceiveChan)
            maybe loop (\line -> outputStrLn line >> interrupt Interrupt) mline

newClientState :: WS.Connection -> IO ClientState
newClientState conn = ClientState conn <$> newMVar (Lounge [] mempty) <*> newMVar Nothing <*> newTChanIO <*> myThreadId

clientEventHandler :: (MonadIO m, MonadReader ClientState m) => Event -> m ()
clientEventHandler (LoungeInfo lounge) = view clientLounge >>= liftIO . (`swapMVar` lounge) >> printLounge
clientEventHandler (StartGame gstate) = view clientGame >>=  liftIO . (`swapMVar` Just gstate) >> startGame
clientEventHandler x = clientPrint ["Unhandled event: " <> show x]

clientInputHandler :: ReaderT ClientState (InputT IO) ()
clientInputHandler = do
    minput <- lift $ getInputLine "% "
    unless (minput == Just "quit") $ do
        case minput of
            Nothing     -> return ()
            Just ""     -> return ()

            Just ('r':'a':'w':' ':input) ->
                maybe (lift $ outputStrLn "Cmd not recognized")
                      (\e -> view clientConn >>= (`unicast` e)) (readMay input)

            Just input
                | "users" `isPrefixOf` input -> printUsers
                | "games" `isPrefixOf` input -> undefined
                | "new"   `isPrefixOf` input -> undefined -- createGame 

            Just x -> lift $ outputStrLn $ "Command `" <> x <> "` not recognized"

clientApp :: WS.ClientApp ()
clientApp conn = do
    state <- newClientState conn
    putStrLn "Connected to server. Type 'help' for available commands."

    listenerThread <- forkIO (runReaderT clientReceiver state)

    unicast conn (JoinServer "whoami")
    runInputT defaultSettings (clientInputLoop state)

    -- cleanup
    unicast conn (PartServer "Bye")
    killThread listenerThread

serverMain :: IO ()
serverMain = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ serverApp state

clientMain :: IO ()
clientMain = WS.runClient "localhost" 9160 "/" clientApp

-- | Documentation for 'main'
main :: IO ()
main = do
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
        client = putStrLn "Starting client..." >> clientMain
