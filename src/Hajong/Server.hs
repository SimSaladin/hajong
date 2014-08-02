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

import           Control.Concurrent
import           Control.Concurrent.Async
import           ClassyPrelude              hiding (finally, handle, toLower)
import           Control.Lens
import           Control.Monad.Reader       (runReaderT, ReaderT, MonadReader)
import           Control.Monad.Trans.Either
import           Data.Set                   (mapMonotonic)
import qualified Data.Text                  as T
import           System.Console.Haskeline   hiding (throwIO)
import qualified Network.WebSockets         as WS

----------------------------------------------------
import Hajong.Game

-- * Types

type Nick = Text
data Client = Client Nick WS.Connection
instance Eq Client where Client a _ == Client b _ = a == b
instance Ord Client where Client a _ <= Client b _ = a <= b

newtype Worker a = Worker { runWorker :: ReaderT WorkerState IO a }
                   deriving (Monad, MonadIO, MonadReader WorkerState)

type WorkerState = (TVar (GameState Client), WorkerChan)

type WorkerChan = TMVar (Either (Worker ()) (Maybe Client, TurnAction))

data ServerState = ServerState
                 { _serverConnections :: Map Nick (WS.Connection, Maybe Int)
                 , _serverLounge :: Set Client
                 , _serverGames :: Map Int WorkerChan
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
    client@(Client nick _conn) <- viewClient
    liftIO $ putStrLn $ "Client disconnected (" <> nick <> ")"
    state <- withSSAtomic $ \var -> do
        modifyTVar var
            $ over serverConnections     (deleteMap nick)
            . over serverLounge          (deleteSet client)
            -- TODO
            -- . over (serverGames.each.gamePlayers.each._2) (\x -> if x == Just client then Nothing else x)
        readTVar var
    broadcast (PartServer nick) state

-- * Communication

-- | In lounge
broadcast :: MonadIO m => Event -> ServerState -> m ()
broadcast event state = liftIO $ forM_ (state ^. serverLounge) cast
    where
        cast = (`unicast` event) . getConn

multicast :: MonadIO m => GameState Client -> Event -> m ()
multicast gs event = forM_ (gs^.gamePlayers^..(each._2._Just)) $ (`unicast` event) . getConn

unicast :: MonadIO m => WS.Connection -> Event -> m ()
unicast conn = liftIO . WS.sendTextData conn

-- * Functions

buildLounge :: ServerState -> Lounge
buildLounge = Lounge
    <$> view (serverLounge . to (mapMonotonic getNick)) -- Idle
    <*> pure mempty -- TODO
--     <*> over each ((,) <$> _gameName <*> setFromList . (^..each._2._Just.to getNick) . _gamePlayers) . view serverGames

runServer :: TVar ServerState -> Client -> Server a -> IO a
runServer tvar client sa = runReaderT sa (tvar, client)

-- * Games

createGame :: Text -> Server ()
createGame name = do
    client@(Client nick conn) <- viewClient

    workerVar <- liftIO newEmptyTMVarIO

    res <- withSSAtomic $ \var -> do
        state <- readTVar var

        let counter  = state ^. serverCounter
            newState = state
                & over serverGames (insertMap counter workerVar)
                . over serverCounter (+1)
                . over serverLounge (deleteSet client)

        if state ^. serverLounge.to (member client)
            then do
                writeTVar var newState
                return $ Just (newState, counter)
            else return Nothing

    case res of
        Just (state, counter) -> do
            let Just gs = newGameState name & gsAddPlayer client -- "Just" because starts with empty players
            liftIO $ startWorker workerVar gs
            broadcast (NewGame (counter, name, singletonSet nick)) state
            unicast conn $ JoinGame counter nick
        Nothing -> unicast conn $ Invalid "Already in a game or waiting for one, cannot create new one"

-- | Pass the TA to relevant worker
handleGameAction :: TurnAction -> Server ()
handleGameAction ta = do
    client   <- viewClient
    received <- withSSAtomic $ \var -> do
        state <- readTVar var
        let Just n = state ^? gameId (getNick client)
        case state ^. gameAt n of
            Just goVar -> tryPutTMVar goVar (Right (Just client, ta))
            Nothing -> return False

    -- TODO receive or not notify
    return ()

joinGame :: Int -> Server ()
joinGame n = do
    client@(Client nick conn) <- viewClient
    ssVar <- view _1

    res <- withSSAtomic $ \var -> runEitherT $ do
        state <- lift $ readTVar var
        when (state^.serverLounge.to (not . member client)) $ left "Already in a game. You cannot join multiple games"
        goVar <- maybeToEitherT "Game not found" $ state^.gameAt n
        return (goVar, state)

    case res of
        Left err -> unicast conn $ Invalid err
        Right (goVar, ss) -> do

            success <- lift $ atomically $ tryPutTMVar goVar $ Left $ do
                gsVar <- view _1

                gsRes <- atomically $ runEitherT $ do
                    gs  <- lift $ readTVar gsVar
                    gs' <- maybeToEitherT "Game is full" $ gsAddPlayer client gs
                    lift $ writeTVar gsVar gs'
                    lift $ writeTVar ssVar $ ss & serverLounge %~ deleteSet client
                    return gs

                case gsRes of
                    Left err -> unicast conn $ Invalid err
                    Right gs -> do
                        broadcast (JoinGame n nick) ss
                        maybeBeginGame n gs
            return ()

-- * Worker

-- | Start the game worker thread
startWorker :: WorkerChan -> GameState Client -> IO ()
startWorker receiveVar gs = do
    gsVar <- newTVarIO gs
            
    _ <- forkIO $ runReaderT (runWorker workerWaitSuccess) (gsVar, receiveVar)
    return ()

-- | Take the element from input queue.
workerTake :: Worker (Either (Worker ()) (Maybe Client, TurnAction))
workerTake = liftIO . atomically . takeTMVar =<< view _2

-- | Wait until succesful turn action.
workerWaitSuccess :: Worker ()
workerWaitSuccess = join workerUntilTurnAction

-- | Operate until one valid turn action is encountered. The ta induced
-- continuation is returned.
workerUntilTurnAction :: Worker (Worker ())
workerUntilTurnAction = workerTake >>= either (>> workerUntilTurnAction) (`workerAction` workerUntilTurnAction)

-- | Attempt to apply the turn action; if failed call the callback.
workerAction :: (Maybe Client, TurnAction) -> Worker (Worker ()) -> Worker (Worker ())
workerAction (mc, ta) whenLeft = do
    gs <- liftIO . readTVarIO =<< view _1
    case workerProcessTurnAction ta mc gs of
        Left _err -> whenLeft -- TODO send err to client
        Right act -> return act

workerProcessTurnAction :: TurnAction -> Maybe Client -> GameState Client -> Either Text (Worker ())
workerProcessTurnAction ta mc gs = do

    -- Solve client to player
    (mclient, player) <- case mc of
                           Just _ -> do
                                let (pl,_,_):_ = gs ^. gamePlayers & filter (^._2.to (== mc))
                                return (mc, pl)
                           Nothing -> do
                                let Just turnPlayer = gs ^? gameRound._Just.riichiPublic.riichiTurn
                                let (_,mc,_):_ = gs ^. gamePlayers & filter (^._1.to (== turnPlayer))
                                return (mc, turnPlayer)

    -- Execute the game action
    (mhand, secret, events) <- _Left %~ ("Game error: " <>) $ gsRoundAction (runTurn player ta) gs

    return $ do
        gsVar <- view _1
        atomically $ writeTVar gsVar $ gs & gameRound._Just.riichiSecret .~ secret

        case (mclient, mhand) of
            (Just client, Just hand) -> unicast (getConn client) $ GameHandChanged hand
            _                        -> return ()

        multicast gs (GameEvents events)

        -- the continuation
        case ta of
            TurnTileDraw _ _ -> waitForAction
            TurnAnkan _ -> waitForAction
            TurnTileDiscard _ _ | Just wp <- _riichiWaitShoutsFrom secret -> waitForShouts wp
            TurnShouted _ _     | Just wp <- _riichiWaitShoutsFrom secret -> waitForShouts wp
            _ -> advanceTurn

-- | Allow the turn to advance.
advanceTurn :: Worker ()
advanceTurn = undefined

-- | Wait for the given players to either initialize a shout or confirm
-- no-shout, or timeout and advance the round normally.
waitForShouts :: [Player] -> Worker ()
waitForShouts players = undefined

-- | Expect an action in the queue, or timeout and automatically end the
-- turn with a default action.
waitForAction :: Worker ()
waitForAction = undefined

-- | Apply a single turn action or call a worker if timed out.
workerApplyTurnActionOrTimeout :: Int -> Worker () -> Worker ()
workerApplyTurnActionOrTimeout n whenOut = do
    s <- view id
    liftIO $ race_ (runReaderT (runWorker workerUntilTurnAction) s) (threadDelay n >> runReaderT (runWorker whenOut) s)

-- | Begin the game including the first round if all players have joined
-- and the game is not yet started.
maybeBeginGame :: Int -> GameState Client -> Worker ()
maybeBeginGame n = maybe (return ()) (liftIO >=> handleBeginGame n) . gsMaybeFirstRound

handleBeginGame :: Int -> GameState Client -> Worker ()
handleBeginGame n gs = do
    gv <- view _1
    atomically $ writeTVar gv gs

    forM_ (gs^.gamePlayers) $ \(player, Just (Client _ conn), _) ->
        unicast conn $ StartGame
            $ playerPlayers.each._2._Just %~ getNick -- drop conn
            $ gsPlayerLookup gs player ^?! _Just

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
--gameAt :: Functor f => Int -> (Maybe (GameState Client) -> f (Maybe (GameState Client))) -> ServerState -> f ServerState
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
