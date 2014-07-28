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

import           ClassyPrelude              hiding (finally, handle, toLower)
import           Control.Lens
import           Control.Monad.Reader       (runReaderT, ReaderT, MonadReader)
import           Control.Monad.Trans.Either
import           Data.Set                   (mapMonotonic)
import qualified Data.Text                  as T
import           System.Console.Haskeline   hiding (throwIO)
import qualified Network.WebSockets         as WS

----------------------------------------------------
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
                 , _serverGames :: Map Int (GameState Client)
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
           | Invalid Text
           -- Game events
           | CreateGame Text
           | NewGame (Int, Text, Set Nick) -- name, nicks
           | StartGame (GamePlayer Nick)
           | JoinGame Int Text -- ^ Game lounge
           -- In-game events
           | GameAction TurnAction -- ^ From client to server only
           | GameEvents [RoundEvent]
           | GameHandChanged Hand -- Own hand only
           | GameShout Shout
           deriving (Show, Read)

instance WS.WebSocketsData Event where
    fromLazyByteString = fromMaybe (Invalid "Malformed event") . readMay . T.unpack . WS.fromLazyByteString
    toLazyByteString = WS.toLazyByteString . T.pack . show

data PartedException = PartedException deriving (Show, Typeable)
instance Exception PartedException

makeLenses ''Lounge
makeLenses ''ServerState

type Server = ReaderT (TVar ServerState, Client) IO

-- * Utility

getNick :: Client -> Nick
getNick (Client nick _) = nick

getConn :: Client -> WS.Connection
getConn (Client _ conn) = conn

newServerState :: ServerState
newServerState = ServerState mempty mempty mempty 0

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
            run onClientConnect `finally` run onClientDisconnect

        _ -> do
            putStrLn $ pack $ "Received non-event first: " <> show event
            unicast conn (Invalid "No JoinServer received. Please identify yourself.")

-- | Execute usual connection rituals with the new connection.
onClientConnect :: Server ()
onClientConnect = do
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
            talkClient
        Nothing -> unicast conn nickTaken
    where
       nickTaken = Invalid "Nick already in use"

-- | Client disconnect rituals.
onClientDisconnect :: Server ()
onClientDisconnect = do
    client@(Client nick conn) <- viewClient
    liftIO $ putStrLn $ "Client disconnected (" <> nick <> ")"
    state <- withSSAtomic $ \var -> do
        modifyTVar var
            $ over serverConnections     (deleteMap nick)
            . over serverLounge          (deleteSet client)
            . over (serverGames.each.gamePlayers.each._2) (\x -> if x == Just client then Nothing else x)
        readTVar var
    broadcast (PartServer nick) state

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

-- * Functions

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (serverLounge . to (mapMonotonic getNick)) -- Idle
    <*> over each ((,) <$> _gameName <*> setFromList . (^..each._2._Just.to getNick) . _gamePlayers) . view serverGames

runServer :: TVar ServerState -> Client -> Server a -> IO a
runServer tvar client sa = runReaderT sa (tvar, client)

-- * Handling games

-- | Begin the game including the first round if all players have joined
-- and the game is not yet started.
maybeBeginGame :: Int -> GameState Client -> Server ()
maybeBeginGame n = maybe (return ()) (liftIO >=> handleBeginGame n) . gsMaybeFirstRound

handleBeginGame :: Int -> GameState Client -> Server ()
handleBeginGame n gs = do
    forM_ (gs^.gamePlayers) $ \(player, Just (Client _ conn), _) ->
        unicast conn $ StartGame
            $ playerPlayers.each._2._Just %~ getNick -- drop conn
            $ gsPlayerLookup gs player ^?! _Just
    withSSAtomic (`modifyTVar` (gameAt n .~ Just gs))

-- | Do stuff on received TurnAction: notify of illegal event if game not
-- found, otherwise info from game server.
handleGameAction :: TurnAction -> Server ()
handleGameAction turnAction = do
    client@(Client nick conn) <- viewClient

    res <- withSSAtomic $ \var -> runEitherT $ do
        state <- lift $ readTVar var
        gid   <- maybeToEitherT "Not in a game"        $ state ^? gameId nick
        deal  <- maybeToEitherT "Game is not on-going" $ state ^. gameAt gid

        -- Find the player info of the client
        let [(player,_,_)] = filter (^._2.to (== Just client)) $ _gamePlayers deal

        -- Execute the game action
        (mhand, secret, events) <- hoistEither
            $ _Left %~ ("Game error: " <>)
            $ gsAction (runTurn player turnAction) deal

        -- Write changes to server state
        lift $ writeTVar var $ state & gameAt gid._Just.gameRound._Just.riichiSecret .~ secret
        return (mhand, gid, state, events)

        -- Return results to broadcast
        return (mhand, gid, state, events)

    let broadcastEvents (mhand, gid, state, events) = do
            maybe (return ()) (unicast conn . GameHandChanged) mhand
            multicast gid (GameEvents events) state

    either (unicast conn . Invalid) broadcastEvents res

-- * Handling clients

talkClient :: Server ()
talkClient = do
    Client nick conn <- viewClient
    forever $ do
        event <- liftIO $ WS.receiveData conn
        case event of
            PartServer reason -> viewSS' >>= broadcast (Message "" $ "User " <> nick <> " has left [" <> reason <> "]") >> liftIO (throwIO PartedException)
            Message _ msg     -> viewSS' >>= broadcast (Message nick msg)
            CreateGame name   -> createGame name
            JoinGame n _      -> joinGame n
            GameAction a      -> handleGameAction a
            JoinServer _      -> unicast conn (Invalid "Already joined (and nick change not implemented)")
            _ -> do
                unicast conn (Invalid "Event not allowed or not implemented.")
                liftIO $ print $ "[ignored event] " <> show event

createGame :: Text -> Server ()
createGame name = do
    client@(Client nick conn) <- viewClient
    res <- withSSAtomic $ \var -> do
        state <- readTVar var
        if state ^. serverLounge.to (member client)
            then do
                let counter         = state ^. serverCounter
                    Just gameServer = newGameState name & gsAddPlayer client -- "Just" because starts with empty players
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

joinGame :: Int -> Server ()
joinGame n = do
    client@(Client nick conn) <- viewClient

    res <- withSSAtomic $ \var -> runEitherT $ do
        state <- lift $ readTVar var
        when (state^.serverLounge.to (not . member client))
            $ left "Already in a game. You cannot join multiple games"

        gs <- maybeToEitherT "Game not found" $ state^.gameAt n

        when (Just client `elem` (gs^.gamePlayers^..(each._2)))
            $ left "Already in the game"

        gs' <- maybeToEitherT "Game is full" $ gsAddPlayer client gs

        let newState = state & set (serverGames.at n.traversed) gs'
                             . over serverLounge (deleteSet client)
        lift $ writeTVar var newState
        return (newState, gs')

    either (unicast conn . Invalid) (handleComm nick) res
    where
        handleComm nick (state, gs) = do
            broadcast (JoinGame n nick) state  -- join confirmation
            maybeBeginGame n gs

-- * Helpers

-- | The endpoint client
viewClient :: Server Client
viewClient = view _2

viewSS :: Server (TVar ServerState)
viewSS = view _1

-- | Read the ServerState TVar
viewSS' :: Server ServerState
viewSS' = viewSS >>= liftIO . readTVarIO

withSSAtomic :: (TVar ServerState -> STM a) -> Server a
withSSAtomic f = viewSS >>= atomically . f

-- gameId :: Nick -> Getting (First Int) ServerState Int
gameId nick = serverConnections.at nick._Just._2._Just

-- |
gameAt n = serverGames.at n

-- | convert maybe to EitherT
maybeToEitherT :: Monad m => e -> Maybe a -> EitherT e m a
maybeToEitherT def = maybe (left def) return

-- ** Dump

dumpState :: ServerState -> IO ()
dumpState state = print
    ( state ^. serverCounter
    , state ^. serverLounge & mapMonotonic getNick
--    , state ^. serverGames & over (each._3) (mapMonotonic getNick)
    )

viewTuple :: (Applicative f, MonadReader s f) => Getting a1 s a1 -> Getting a s a -> f (a1, a)
viewTuple l m = (,) <$> view l <*> view m
