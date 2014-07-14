{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Server
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Server where

import           ClassyPrelude hiding (finally, handle, toLower)
import           Data.Set (mapMonotonic)
import           Control.Lens
import           Control.Monad.Reader (runReaderT, ReaderT, MonadReader)
import qualified Data.Text as T
import           System.Console.Haskeline hiding (throwIO)
import qualified Network.WebSockets as WS
import           Control.Monad.Trans.Either

import Hajong.Game.Mechanics
import Hajong.Game.Types

-- * Types

type Nick = Text
data Client = Client Nick WS.Connection
instance Eq Client where Client a _ == Client b _ = a == b
instance Ord Client where Client a _ <= Client b _ = a <= b

data ServerState = ServerState
                 { _serverConnections :: Map Nick (WS.Connection, Maybe Int)
                 , _serverLounge :: Set Client
                 , _serverGames :: Map Int (GameServer Client)
                 , _serverCounter :: Int
                 }

data Lounge = Lounge
            { _loungeNicksIdle :: Set Nick
            , _loungeGames :: Map Int (Text, Set Nick)
            } deriving (Show, Read)

data Event = JoinServer Text -- ^ Nick
           | PartServer Text
           | LoungeInfo Lounge
           | Message Text Text -- ^ from, content
           | CreateGame Text
           | NewGame (Int, Text, Set Nick) -- name, nicks
           | StartGame (GamePlayer Nick)
           | JoinGame Int Text -- ^ Game lounge
           | GameAction TurnAction
           | GameEvents [RoundEvent]
           | GameHandChanged Hand
           | GameShout Shout
           | Invalid Text
           deriving (Show, Read)
instance WS.WebSocketsData Event where
    fromLazyByteString = fromMaybe (Invalid "Malformed event") . readMay . T.unpack . WS.fromLazyByteString
    toLazyByteString = WS.toLazyByteString . T.pack . show

data PartedException = PartedException deriving (Show, Typeable)
instance Exception PartedException

makeLenses ''Lounge
makeLenses ''ServerState

getNick :: Client -> Nick
getNick (Client nick _) = nick

getConn :: Client -> WS.Connection
getConn (Client _ conn) = conn

newServerState :: ServerState
newServerState = ServerState mempty mempty mempty 0

type Server = ReaderT (TVar ServerState, Client) IO

-- * Communication

broadcast :: MonadIO m => Event -> ServerState -> m ()
broadcast event state = liftIO $ do
    forM_ (state ^. serverLounge) cast
    forMOf_ (serverGames.traversed.gamePlayers.each._2._Just) state cast
    where
        cast = (`unicast` event) . getConn

multicast :: MonadIO m => Int -> Event -> ServerState -> m ()
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
        JoinServer nick -> do
            let client = Client nick conn
                run    = runServer stateVar client
            putStrLn $ "New client " <> nick
            run serverConnect `finally` run serverDisconnect

        _ -> do
            putStrLn $ pack $ "Received non-event first: " <> show event
            unicast conn (Invalid "No JoinServer received. Please identify yourself.")

serverConnect :: Server ()
serverConnect = do
    client@(Client nick conn) <- viewClient
    accepted <- withSSAtomic $ \var -> do
        state <- readTVar var
        if state ^. serverConnections.at nick.to isNothing
            then let newState = state
                        & over serverConnections (insertMap nick (conn, Nothing))
                        . over serverLounge (insertSet client)
                    in writeTVar var newState >> return (Just newState)
            else return Nothing -- nick taken
    case accepted of
        Just s  -> do
            unicast conn $ LoungeInfo $ buildLounge s
            broadcast (JoinServer nick) s
            talk
        Nothing -> unicast conn nickTaken
    where
       nickTaken = Invalid "Nick already in use"

serverDisconnect :: Server ()
serverDisconnect = do
    client@(Client nick conn) <- viewClient
    liftIO $ putStrLn $ "Client disconnected (" <> nick <> ")"
    state <- withSSAtomic $ \var -> do
        modifyTVar var
            $ over serverConnections     (deleteMap nick)
            . over serverLounge          (deleteSet client)
            . over (serverGames.each.gamePlayers.each._2) (\x -> if x == Just client then Nothing else x)
        readTVar var
    broadcast (PartServer nick) state

-- * Functions

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (serverLounge . to (mapMonotonic getNick)) -- Idle
    <*> over each ((,) <$> _gameName <*> setFromList . (^..each._2._Just.to getNick) . _gamePlayers) . view serverGames

runServer :: TVar ServerState -> Client -> Server a -> IO a
runServer tvar client sa = runReaderT sa (tvar, client)

viewClient :: Server Client
viewClient = view _2

viewSS :: Server (TVar ServerState)
viewSS = view _1

-- | Read server state tvar
viewSS' :: Server ServerState
viewSS' = viewSS >>= liftIO . readTVarIO

withSSAtomic :: (TVar ServerState -> STM a) -> Server a
withSSAtomic f = viewSS >>= atomically . f

-- gameId :: Nick -> Getting (First Int) ServerState Int
gameId nick = serverConnections.at nick._Just._2._Just
gameAt n = serverGames.at n

-- * Logic

talk :: Server ()
talk = do
    Client nick conn <- viewClient
    let loop :: Server ()
        loop = do
            event <- liftIO $ WS.receiveData conn
            state <- liftIO . readTVarIO =<< viewSS
            case event of
                JoinServer _      -> unicast conn (Invalid "Already joined (and nick change not implemented)")
                PartServer reason -> broadcast (Message "" $ "User " <> nick <> " has left [" <> reason <> "]") state >> liftIO (throwIO PartedException)
                Message _ msg     -> broadcast (Message nick msg) state
                CreateGame name   -> serverCreateGame name
                JoinGame n _      -> serverJoinGame n
                GameAction a      -> handleGameAction a
                _ -> do
                    unicast conn (Invalid "Event not allowed or not implemented.")
                    liftIO $ print $ "[ignored event] " <> show event
    forever loop

serverCreateGame :: Text -> Server ()
serverCreateGame name = do
    client@(Client nick conn) <- viewClient
    res <- withSSAtomic $ \var -> do
        state <- readTVar var
        if state ^. serverLounge.to (member client)
            then do
                let counter         = state ^. serverCounter
                    Just gameServer = newGameServer name & gsAddPlayer client -- "Just" because starts with empty players
                    newState        = state
                        & over serverGames (insertMap counter gameServer)
                        . over serverCounter (+1)
                        . over serverLounge (deleteSet client)
                writeTVar var newState
                return $ Just (newState, counter)
            else return Nothing
    case res of
        Just (state, counter) -> do
            broadcast (NewGame (counter, name, singletonSet nick)) state
            unicast conn $ JoinGame counter nick
        Nothing -> unicast conn $ Invalid "Already in a game or waiting for one, cannot create new one"

serverJoinGame :: Int -> Server ()
serverJoinGame n = do
    client@(Client nick conn) <- viewClient
    res <- withSSAtomic $ \var -> runEitherT $ do
        state <- lift $ readTVar var
        when (state^.serverLounge.to (not . member client)) (left "Already in a game. You cannot join multiple games")

        gs <- maybeToEitherT "Game not found" $ state^.serverGames.at n

        when (Just client `elem` (gs^.gamePlayers^..(each._2))) $ left "Already in the game"

        gs' <- maybeToEitherT "Game is full" $ gsAddPlayer client gs

        let newState = state & set (serverGames.at n.traversed) gs'
                             . over serverLounge (deleteSet client)
        lift $ writeTVar var newState
        return (newState, advanceGameState gs')

    either (unicast conn . Invalid) (handleComm nick) res
    where
        handleComm nick (state, advance) = do
            broadcast (JoinGame n nick) state  -- join confirmation
            res <- advance
            case res of
                Just gs -> withSSAtomic $ \var ->
                    modifyTVar var $ set (serverGames.at n) $ Just gs
                Nothing -> return ()


-- | Documentation for 'advanceGameState'
advanceGameState :: GameServer Client -> Server (Maybe (GameServer Client))
advanceGameState gs = case gsNewGame gs of
    Just gsIO -> do
        gs <- liftIO gsIO
        let handlePlayer (player, Just (Client _ conn), _) = unicast conn
                $ StartGame
                $ playerPlayers.each._2._Just %~ getNick -- drop conn
                $ gsPlayerLookup gs player ^?! _Just
            handlePlayer (_,_,_) = error "The impossible happened"
        mapM_ handlePlayer (gs^.gamePlayers)
        return $ Just gs
    Nothing -> return Nothing -- XXX: turn advancements

-- | Do stuff on received TurnAction: notify of illegal event if game not
-- found, otherwise info from game server.
handleGameAction :: TurnAction -> Server ()
handleGameAction turnAction = do
    client@(Client nick conn) <- viewClient

    res <- withSSAtomic $ \var -> runEitherT $ do
        state              <- lift $ readTVar var
        gid                <- maybeToEitherT "Not in a game"        $ state ^? gameId nick
        deal               <- maybeToEitherT "Game is not on-going" $ state ^. gameAt gid

        let [(player,_,_)] = filter (^._2.to (== Just client)) $ _gamePlayers deal

        (mhand, secret, events) <- hoistEither
            $ _Left %~ ("Game error: " <>)
            $ gsAction (runTurn player turnAction) deal

        lift $ writeTVar var $ state & gameAt gid._Just.gameState._Just._1 .~ secret
        return (mhand, gid, state, events)

    either (unicast conn . Invalid) broadcastEvents res
    where
        broadcastEvents (mhand, gid, state, events) = do
            Client _ conn <- viewClient
            maybe (return ()) (unicast conn . GameHandChanged) mhand
            multicast gid (GameEvents events) state


-- * Helpers

dumpState :: ServerState -> IO ()
dumpState state = print
    ( state ^. serverCounter
    , state ^. serverLounge & mapMonotonic getNick
--    , state ^. serverGames & over (each._3) (mapMonotonic getNick)
    )

viewTuple :: (Applicative f, MonadReader s f) => Getting a1 s a1 -> Getting a s a -> f (a1, a)
viewTuple l m = (,) <$> view l <*> view m

-- | convert maybe to either
maybeToEitherT :: Monad m => e -> Maybe a -> EitherT e m a
maybeToEitherT def = maybe (left def) return
