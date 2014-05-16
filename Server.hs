module Server where

import Riichi

import           ClassyPrelude hiding (finally, handle, toLower)
import           Data.Set (mapMonotonic)
import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import qualified Data.Text as T
import           System.Console.Haskeline
import qualified Network.WebSockets as WS
import           Control.Monad.Trans.Either

-- * Types

type Nick = Text

data Client = Client Nick WS.Connection

instance Eq Client where Client a _ == Client b _ = a == b
instance Ord Client where Client a _ <= Client b _ = a <= b

data ServerState = ServerState
                 { _serverConnections :: Map Nick WS.Connection
                 , _serverLounge :: Set Client
                 , _serverGames :: Map Int (GameServer Client)
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
           | StartGame (GamePlayer Nick)
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
broadcast event state = do
    forM_ (state ^. serverLounge) cast
    forMOf_ (serverGames.traversed.gamePlayers.each._2._Just) state cast
    where
        cast = (`unicast` event) . getConn

multicast :: Int -> Event -> ServerState -> IO ()
multicast nth event state =
    case state ^. serverGames . at nth of
        Nothing -> putStrLn "ERROR (multicast): game not found"
        Just gs -> forM_ (gs^.gamePlayers^..(each._2._Just)) $ (`unicast` event) . getConn

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
                        . over (serverGames.each.gamePlayers.each._2) (\x -> if x == Just client then Nothing else x)
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
buildLounge = Lounge
    <$> view (serverLounge . to (mapMonotonic getNick)) -- Idle
    <*> over each ((,) <$> _gameName <*> setFromList . (^..each._2._Just.to getNick) . _gamePlayers) . view serverGames

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
addToGame n client@(Client nick conn) stateVar = 
    atomically (readTVar stateVar >>= atomicLogic)
    >>= either (unicast conn . Invalid) handleComm
    where
        atomicLogic state = runEitherT $ do
            gs <- maybe (left "Game not found") return (state^.serverGames.at n)

            when (Just client `elem` (gs^.gamePlayers^..(each._2))) $
                left "Already in the game"

            gs' <- maybe (left "Game is full") return $ gsAddPlayer client gs

            let state' = state & set (serverGames.at n.traversed) gs'
                               . over serverLounge (deleteSet client)

            lift $ writeTVar stateVar state'
            return state'

        handleComm state = do
            broadcast (JoinGame n nick) state  -- join confirmation

            let Just gs = state^.serverGames.at n

                handlePlayer (player, Just (Client _ conn'), _) = unicast conn'
                    $ StartGame $ gsPlayerLookup gs player ^?! _Just
                    & over (playerPlayers.each._2._Just) getNick
                handlePlayer (_,_,_)                            = return ()

            when (gs^.gamePlayers & find (isn't _Just.view _2) &isNothing) $
                mapM_ handlePlayer (gs^.gamePlayers)

serverCreateGame :: Client -> Text -> TVar ServerState -> IO ()
serverCreateGame client@(Client nick conn) name stateVar = do
    game <- newRiichiState
    state <- atomically $ do
        state <- readTVar stateVar

        let counter         = state ^. serverCounter
            Just gameServer = newGameServer name & gsAddPlayer client -- Just because starts with empty players
            state'          = state
                & over serverGames (insertMap counter gameServer)
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
--    , state ^. serverGames & over (each._3) (mapMonotonic getNick)
    )

viewTuple :: (Applicative f, MonadReader s f) => Getting a1 s a1 -> Getting a s a -> f (a1, a)
viewTuple l m = (,) <$> view l <*> view m
