module Server where

import Riichi

import           ClassyPrelude hiding (finally, handle, toLower)
import           Data.Set (mapMonotonic, size)
import           Control.Lens
import           Control.Monad (zipWithM_)
import           Control.Monad.Reader (MonadReader)
import qualified Data.Text as T
import           System.Random.Shuffle
import           System.Console.Haskeline
import qualified Network.WebSockets as WS

-- * Types

type Nick = Text

data Client = Client Nick WS.Connection

instance Eq Client where Client a _ == Client b _ = a == b
instance Ord Client where Client a _ <= Client b _ = a <= b

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
           | StartGame (RiichiPlayer, [Nick]) -- play order
           | JoinGame Int Text -- ^ Game lounge
           | GameAction TurnAction
           | GameShout Shout
           | Invalid Text
           deriving (Show, Read)

instance WS.WebSocketsData Event where
    fromLazyByteString = fromMaybe (Invalid "Malformed event") . readMay . T.unpack . WS.fromLazyByteString
    toLazyByteString = WS.toLazyByteString . T.pack . show

makeLenses ''ServerState

getNick :: Client -> Nick
getNick (Client nick _) = nick

getConn :: Client -> WS.Connection
getConn (Client _ conn) = conn

newServerState :: ServerState
newServerState = ServerState mempty mempty mempty 0

-- * Communication

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

-- * App

serverMain :: IO ()
serverMain = do
    state <- atomically $ newTVar newServerState
    WS.runServer "0.0.0.0" 9160 $ serverApp state

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
    res <- atomically $ do
        state <- readTVar stateVar
        case state ^. serverGames.at n of

            Nothing -> return (Left "Game not found")

            Just (_, _, clients)
                | client `member` clients -> return $ Left "Already in that game"

                | size clients >= 4 -> return $ Left "That game is full"

                | otherwise -> do
                    let state' = state
                            & over (serverGames.at n.traversed._3) (insertSet client)
                            . over serverLounge (deleteSet client)
                    writeTVar stateVar state'
                    return $ Right (size clients >= 3, state')

    either (unicast conn . Invalid) handleGameAdd res
    where
        handleGameAdd (starting, state) = do
            broadcast (JoinGame n nick) state
            when starting $ do
                let Just (riichiState, clients) = state^.serverGames.at n <&> viewTuple _2 _3

                clients' <- shuffleM $ setToList clients

                let nickOrder = map getNick clients'
                    handlePlayer player (Client _ conn') =
                        unicast conn' $ StartGame (riichiPlayer riichiState player ^?! _Just, nickOrder)

                zipWithM_ handlePlayer defaultPlayers clients'

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

-- * Helpers

dumpState :: ServerState -> IO ()
dumpState state = print
    ( state ^. serverCounter
    , state ^. serverLounge & mapMonotonic getNick
    , state ^. serverGames & over (each._3) (mapMonotonic getNick)
    )

viewTuple :: (Applicative f, MonadReader s f) => Getting a1 s a1 -> Getting a s a -> f (a1, a)
viewTuple l m = (,) <$> view l <*> view m
