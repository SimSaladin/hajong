{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Worker
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Worker thread abstracts over a single game.
--
--  - It takes user actions (one at a time) via a TMVar.
--  - Applies the actions within the game.
--  - Takes care of timeouts and confirmations after a discard, and
--  timeouts players inactive on their turn (and applies some random action
--  to end his turn).
--
------------------------------------------------------------------------------
module Hajong.Worker
    ( WorkerData(..), wGame, wInput, wLogger
    , WorkerInput(..), WorkerResult
    , startWorker
    ) where

import           Hajong.Connections
import           Hajong.Client
import           Mahjong
------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad.Logger
import           Control.Monad.Trans.Control        (MonadBaseControl(..))
import           Control.Monad.Base                 (MonadBase)
import           Control.Concurrent.Async
import           Data.Maybe (fromJust)
import           System.Log.FastLogger
------------------------------------------------------------------------------
default (Text)

-- * Types and lenses

-- type WorkerState = (TVar (GameState Client), TMVar WorkerInput)
data WorkerData = WorkerData
                { _wGame    :: TVar (GameState Client)
                , _wInput   :: TMVar WorkerInput -- ^ Input from outside worker
                , _wLogger  :: LoggerSet
                } deriving (Typeable)

data WorkerInput = WorkerAddPlayer Client Callback -- ^ Careful with the callbacks, an uncatched exception will kill the worker
                 | WorkerPartPlayer Client Bool Callback -- ^ The Bool is whether the player should be completely removed from the game.
                 | WorkerGameAction Client GameAction
                 | WorkerForceStart
                 | WorkerReplaceKyoku Machine (Maybe Kyoku) -- ^ For debugging only.

-- | Result from a dying worker thread.
type WorkerResult = Either SomeException FinalPoints -- ^ Left only on an unexpected event, a bug.

newtype Worker a = Worker { runWorker :: LoggingT (ReaderT WorkerData IO) a }
                   deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader WorkerData, MonadBase IO)

instance MonadBaseControl IO Worker where -- associated type, cannot derive
    type StM Worker a = a
    liftBaseWith f    = Worker $ liftBaseWith $ \q -> f (q . runWorker)
    restoreM          = Worker . restoreM

-- | Because the worker lives as a separete thread, we provide a callback
-- with this type for when the worker dies.
type Finalize = Either SomeException FinalPoints -> IO ()

type Callback = GameState Client -> Worker ()

-- * Lenses
makeLenses ''WorkerData

-- * Running

-- | Start the worker in a new thread.
startWorker :: Finalize -> WorkerData -> IO ThreadId
startWorker ends wdata = execWorker wdata processMachine `forkFinally` ends

execWorker :: WorkerData -> Worker a -> IO a
execWorker st cont = (runWorker cont `runLoggingT` logger) `runReaderT` st
    where logger loc src level = pushLogStr (st^.wLogger) . defaultLogStr loc src level

-- * Managing connected players

-- | Remove the player identified by connection from the game and replace
-- the player with a dummy client.
partPlayer :: Bool -- ^ Leave permanently; erases the seat info for the client
           -> Client -> Callback -> Worker ()
partPlayer leave client callback = do
    $logInfo $ "Client disconnected: " <> tshow client
    gsv <- view wGame
    mgs <- atomically $ do
        gs <- readTVar gsv

        case clientToPlayer client gs of
            Just p  -> do
                let gs' = if leave then setClient (dummyClient $ getNick client ++ " (n/a)") p gs
                                   else gs & gamePlayers.ix p %~ disconnectClient
                writeTVar gsv gs' $> Just gs'
            Nothing -> return Nothing

    maybe ($logError $ "Parting client " ++ getNick client ++ ", but it doesn't exist.")
          callback mgs

-- | Add a new or existing player to the game with the given connection.
addPlayer :: Client -> Callback -> Worker ()
addPlayer client callback = do
    $logInfo $ "Client connecting: " <> tshow client
    gsv  <- view wGame
    e_gs <- atomically $ runEitherT $ do
        gs  <- lift $ readTVar gsv
        gs' <- case clientToPlayer client gs of
            Nothing -> addClient client gs ? "Game is full"
            Just p  -> return $ setClient client p gs
        lift $ writeTVar gsv gs'
        return gs'

    clientEither client e_gs $ \gs -> do
        when (gs^.gameKyoku.to isJust) $ do
            let p = fromJust $ clientToPlayer client gs -- We check in above transaction the client really is there
            unsafeRoundM $ updatePlayerNick p (getNick client) -- XXX: cannot be in the same transaction because the kyoku machinery wouldn't update until the resulting events are applied, so the state would be out-of-date in the second acation
            unsafeRoundM $ tellPlayerState p
        callback gs

-- * Worker logic

-- | This is called when the worker is ready to take next input either from
-- a client or server (both come in to the same TMVar).
processInput :: WorkerInput -> Worker (Maybe (GameState Client, [GameEvent]))
processInput (WorkerAddPlayer client callback)        = addPlayer client callback        >> return Nothing
processInput (WorkerPartPlayer client leave callback) = partPlayer leave client callback >> return Nothing
processInput WorkerForceStart                         = rmodify wGame (gamePlayers.each %~ \c -> c { isReady = True }) >> return Nothing
processInput (WorkerReplaceKyoku machine kyoku)       = do rmodify wGame $ (gameState .~ machine) . (gameKyoku .~ kyoku)
                                                           $logInfo "Game state was replaced successfully"
                                                           return Nothing
processInput (WorkerGameAction c ga)                  = processGameAction
  where

    -- | If the action would be valid in the kyoku, return the worker action
    -- which applies the action and the corresponding continuation state.
    processGameAction :: Worker (Maybe (GameState Client, [GameEvent]))
    processGameAction = fmap (clientToPlayer c) (rview wGame) >>= \case
        Just p  -> gameActionToKyokuInput p >>= safeStep >>= either (\e -> unicastError c e >> return Nothing) (return . Just)
        Nothing -> unicastError c "You are not playing in this game" >> return Nothing

    -- | Argument player *must* be present in the game.
    gameActionToKyokuInput :: Player -> Worker MachineInput
    gameActionToKyokuInput p = do
        k <- unsafeRoundM (playerToKaze p)
        return $ case ga of
            GameTurn ta  -> InpTurnAction k ta
            GameShout sh -> InpShout k sh
            GameDontCare -> InpPass k

-- ** Waiting for input

-- | Wait until someone inputs a valid action to the kyoku machine
-- (@stepByClient@) or a timeout is reached. After timeout we continue with
-- an @InpAuto@.
stepByClientOrTimeout :: Worker ()
stepByClientOrTimeout = do
    let secs = 15 -- TODO hard-coded limit
    join $ workerRace secs stepByClient
         $ do $logDebug "Time-out, proceeding automatically."
              unsafeStep InpAuto

-- | Wait indefinetely until someone inputs a valid action to the kyoku
-- machine.
stepByClient :: Worker (Worker ())
stepByClient = go
    where go = takeInput >>= processInput >>= maybe go (\(gs, ma) -> return $ void $ sendGameEvents ma >> rswap wGame gs)

-- | Safe kyoku step. Checks whether the step is valid, but does not do
-- any relevant modifications to the worker state. The modifications can be
-- done by running the @Worker@ action of the result.
safeStep :: MachineInput -> Worker (Either Text (GameState Client, [GameEvent]))
safeStep inp = rview wGame <&> roundStep inp
    
unsafeStep :: MachineInput -> Worker ()
unsafeStep inp = safeStep inp >>= either failed go
    where failed e = error $ "unsafeStep: unexpected Left: " <> unpack e
          go (gs, evs) = sendGameEvents evs >> void (rswap wGame gs)

-- * The game machine

-- | Monitor the kyoku machine and perhaps do something in there.
processMachine :: Worker FinalPoints
processMachine = do
    m <- rview wGame <&> view gameState
    logDebugN $ "Transitioned to state " ++ tshow m
    case m of
        KyokuNone                      -> waitPlayersAndBegin
        KyokuStartIn s                 -> workerWait s >> unsafeStep InpAuto >> processMachine
        CheckEndConditionsAfterDiscard -> unsafeStep InpAuto    >> processMachine -- auto check
        WaitingDraw{}                  -> unsafeStep InpAuto    >> processMachine -- auto draw
        WaitingDiscard{}               -> stepByClientOrTimeout >> processMachine
        WaitingShouts{}                -> stepByClientOrTimeout >> processMachine
        KyokuEnded res                 -> processKyokuEnded res
        HasEnded points                -> return points
  where

    processKyokuEnded :: KyokuResults -> Worker FinalPoints
    processKyokuEnded results = do
        $logInfo $ "Kyoku ended: " <> tshow results
        gs <- rview wGame
        let Just k = _Just.pResults.~Just results $ _gameKyoku gs -- TODO shouldn't be here
        nextk <- liftIO (maybeNextDeal k)
        rmodify wGame $ case nextk of
            Left points -> (gameState .~ HasEnded points)
            Right kyoku -> (gameState .~ KyokuStartIn 10) . (gameKyoku.~Just kyoku)
        processMachine

    waitPlayersAndBegin :: Worker FinalPoints
    waitPlayersAndBegin = $logInfo "Waiting for players" >> wait
      where
        wait = do gs <- rview wGame
                  case maybeBeginGame gs of
                      Nothing    -> takeInput >>= processInput >> wait
                      Just to_gs -> liftIO to_gs >>= rswap wGame >> processMachine

-- * Worker utilities

-- | "workerRace n ma b" races between actions "ma" and "threadDelay n >> return b".
workerRace :: Int -> Worker a -> a -> Worker a
workerRace secs ma b = do
    s   <- view id
    res <- liftIO $ execWorker s ma `race` threadDelay (secs * 1000000)
    either return (\_ -> return b) res

workerWait :: Int -> Worker ()
workerWait n = liftIO $ threadDelay (n * 1000000)

-- | Take the next element from input queue (blocking).
takeInput :: Worker WorkerInput
takeInput = liftIO . atomically . takeTMVar =<< view wInput

-- | Send to everyone in game.
multicast :: Event -> Worker ()
multicast ev = do gs <- rview wGame
                  mapM_ (`safeUnicast` ev) (gs^..gamePlayers.each)

-- ** Game-related

-- | See what would happen if the given round action was executed.
roundM :: RoundM a -> Worker (Either Text (a, Worker ()))
roundM ma = rview wGame <&> fmap go . runRoundM ma
    where go (res, gs, evs) = (res, rswap wGame gs >> sendGameEvents evs)

-- | Just execute the action - if it fails the worker dies! Use with care.
unsafeRoundM :: RoundM a -> Worker a
unsafeRoundM = roundM >=> either failed go
  where failed e  = error $ "unsafeRoundM: unexpected Left: " <> unpack e
        go (a, m) = m >> return a

-- | Send private events individually and public events in bulk.
sendGameEvents :: [GameEvent] -> Worker ()
sendGameEvents events = do
    gs <- rview wGame
    public <- filterM (sendPrivates gs) events
    multicast $ InGameEvents public
  where
    sendPrivates gs e@(DealStarts (PlayerKyoku p _ _))  = f gs p e
    sendPrivates gs e@(DealWaitForShout (p,_,_,_))      = f gs p e
    sendPrivates gs e@(DealWaitForTurnAction (p,_,_,_)) = f gs p e
    sendPrivates gs e@(DealPrivateHandChanged p _ _)    = f gs p e
    sendPrivates _ _                                    = return True

    f gs p e = maybe (return ()) (`safeUnicast` InGamePrivateEvent e) (playerToClient gs p) $> False
