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
    ( WorkerData(..), wGame, wSettings, wMachine, wInput, wLogger
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
                { _wSettings  :: GameSettings
                , _wGame      :: TVar (GameState Client)
                , _wMachine   :: TVar Machine
                , _wInput     :: TMVar WorkerInput -- ^ Input from outside worker
                , _wLogger    :: LoggerSet
                } deriving (Typeable)

-- | Result from a dying worker thread.
type WorkerResult = Either SomeException FinalPoints -- ^ Left only on an unexpected event, a bug.

data WorkerInput = WorkerAddPlayer Client (GameState Client -> IO ())
                 | WorkerPartPlayer Client (GameState Client -> IO ())
                 | WorkerGameAction Client GameAction
                 | WorkerForceStart
                 | WorkerReplaceKyoku Machine (Maybe Kyoku) -- ^ For debugging only.

newtype Worker a = Worker { runWorker :: LoggingT (ReaderT WorkerData IO) a }
                   deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader WorkerData, MonadBase IO)

instance MonadBaseControl IO Worker where
    type StM Worker a = a
    liftBaseWith f    = Worker $ liftBaseWith $ \q -> f (q . runWorker)
    restoreM          = Worker . restoreM

-- | When a worker exits gracefully it spits out @FinalPoints@.
type WCont = Worker FinalPoints

-- | Because the worker lives as a separete thread, we provide a callback
-- with this type for when the worker dies.
type Finalize = Either SomeException FinalPoints -> IO ()

-- * Lenses
makeLenses ''WorkerData

-- * Entry points

-- | Start the worker in a new thread.
startWorker :: Finalize -> WorkerData -> IO ThreadId
startWorker ends wdata = runWCont wdata waitPlayersAndBegin `forkFinally` ends

-- * Unwrap monads

-- | Run Worker
runWCont :: WorkerData -> Worker a -> IO a
runWCont st cont = (runWorker cont `runLoggingT` logger) `runReaderT` st
    where logger loc src level = pushLogStr (st^.wLogger) . defaultLogStr loc src level

-- * Update state and emit events

-- | Send to everyone in game.
multicast :: Event -> Worker ()
multicast ev = do gs <- rview wGame
                  mapM_ (`safeUnicast` ev) (gs^..gamePlayers.each)

-- | Send private events individually and public events in bulk.
sendGameEvents :: [GameEvent] -> Worker ()
sendGameEvents events = do
    gs <- rview wGame
    public <- filterM (sendPrivates gs) events
    multicast $ InGameEvents public
  where
    sendPrivates gs e@(DealStarts p _ _)                = f gs p e
    sendPrivates gs e@(DealWaitForShout (p,_,_,_))      = f gs p e
    sendPrivates gs e@(DealWaitForTurnAction (p,_,_,_)) = f gs p e
    sendPrivates gs e@(DealPrivateHandChanged p _ _)    = f gs p e
    sendPrivates _ _                                    = return True

    f gs p e = maybe (return ()) (`safeUnicast` InGamePrivateEvent e) (playerToClient gs p) $> False

-- * Input layer

-- | Take the next element from input queue (blocking).
takeInput :: Worker WorkerInput
takeInput = liftIO . atomically . takeTMVar =<< view wInput

-- | This monster is called when the worker is ready to take next input
-- either from a client or server (both come in to the same TMVar).
--
-- In case of a WorkerGameAction: If it would be valid in the kyoku,
-- return the action which applies the action and the corresponding
-- continuation state.
processInput :: WorkerInput -> Worker (Maybe (Machine, Worker ()))
processInput (WorkerAddPlayer client callback)  = addPlayer client callback >> return Nothing
processInput (WorkerPartPlayer client callback) = partPlayer client callback >> return Nothing

processInput (WorkerGameAction c ga)            = fmap (clientToPlayer c) (rview wGame) >>= \case
    Just p  -> gameActionToKyokuInput p ga >>= safeStep >>= either (\e -> unicastError c e >> return Nothing) (return . Just)
    Nothing -> unicastError c "You are not playing in this game" >> return Nothing

processInput WorkerForceStart                   = rmodify wGame (gamePlayers.each %~ \c -> c { isReady = True }) >> return Nothing

processInput (WorkerReplaceKyoku machine kyoku) = do rmodify wGame (gameDeal .~ kyoku)
                                                     void $ rswap wMachine machine
                                                     $logInfo "Game state was replaced successfully"
                                                     return Nothing

-- * Game play

-- | Begin the game including the first round when all players have joined.
--
--  Internal flow:
--      @waitPlayersAndBegin@
--      -> @beginDeal@
--      -> @processMachine@
--      -> @processKyokuEnded@ (-> @processMachine@)
waitPlayersAndBegin :: WCont
waitPlayersAndBegin = $logInfo "Waiting for players" >> go
  where
    go = rview wGame >>= maybe (takeInput >>= processInput >> go)
                               (liftIO >=> beginGame) . maybeBeginGame

-- | Called when beginning the first kyoku.
beginGame :: GameState Client -> WCont
beginGame gs = do
    $logInfo "Game begins now"
    void $ rswap wGame gs
    unsafeStep InpAuto >>= processMachine

-- | Monitor the kyoku machine and perhaps do something in there.
processMachine :: Machine -> WCont
processMachine m@(NotBegun s)                   = logMachine m >> workerWait s >> processKyokuStarts
processMachine m@CheckEndConditionsAfterDiscard = logMachine m >> unsafeStep InpAuto    >>= processMachine -- auto check
processMachine m@WaitingDraw{}                  = logMachine m >> unsafeStep InpAuto    >>= processMachine -- auto draw
processMachine m@WaitingDiscard{}               = logMachine m >> stepByClientOrTimeout >>= processMachine
processMachine m@WaitingShouts{}                = logMachine m >> stepByClientOrTimeout >>= processMachine
processMachine m@(KyokuEnded res)               = logMachine m >> processKyokuEnded res

logMachine :: Machine -> Worker ()
logMachine m = logDebugN $ "Transsation to state " ++ tshow m

-- | As a precondition, the gamestate must be present in the workerdata.
processKyokuStarts :: WCont
processKyokuStarts = do
    Just k <- rview wGame <&> _gameDeal
    liftIO (maybeNextDeal k) >>= either return go -- returns finalpoints or starts next kyoku
  where
    go k = rmodify wGame (gameDeal.~Just k) >> unsafeStep InpAuto >>= processMachine

processKyokuEnded :: KyokuResults -> WCont
processKyokuEnded results = do
    $logInfo $ "Kyoku ended: " <> tshow results
    rmodify wGame (gameDeal._Just.pResults.~Just results)
    void $ rswap wMachine (NotBegun 15)
    processMachine =<< rview wMachine

-- * Managing connected players 

-- | Remove the player identified by connection from the game and replace
-- the player with a dummy client.
partPlayer :: Client -> (GameState Client -> IO ()) -> Worker ()
partPlayer client callback = do
    $logInfo $ "Client leaving: " <> tshow client
    gsv <- view wGame
    mgs <- atomically $ do
        gs <- readTVar gsv
        case clientToPlayer client gs of
            Just p  -> do
                let gs' = setClient (dummyClient $ getNick client ++ " (n/a)") p gs
                writeTVar gsv gs' $> Just gs'
            Nothing -> return Nothing

    maybe ($logError $ "Parting client " ++ getNick client ++ ", but it doesn't exist.")
          (liftIO . callback) mgs

-- | Add a new or existing player to the game with the given connection.
addPlayer :: Client -> (GameState Client -> IO ()) -> Worker ()
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
        when (gs^.gameDeal.to isJust) $ do
            let p = fromJust $ clientToPlayer client gs -- We check in above transaction the client really is there
            unsafeRoundM $ updatePlayerNick p (getNick client) -- XXX: cannot be in the same transaction because the kyoku machinery wouldn't update until the resulting events are applied, so the state would be out-of-date in the second acation
            unsafeRoundM $ tellPlayerState p
        liftIO $ callback gs

-- * Racing

-- | "workerRace n ma b" races between actions "ma" and "threadDelay n >> return b".
workerRace :: Int -> Worker a -> a -> Worker a
workerRace secs ma b = do
    s   <- view id
    res <- liftIO $ runWCont s ma `race` threadDelay (secs * 1000000)
    either return (\_ -> return b) res

workerWait :: Int -> Worker ()
workerWait n = liftIO $ threadDelay (n * 1000000)

-- * Gameplay-related

-- | See what would happen if the given round action was executed.
roundM :: RoundM a -> Worker (Either Text (a, Worker ()))
roundM ma = do
        gs <- rview wGame
        return $ do (res, kyoku, events) <- runRoundM ma gs
                    return (res, rmodify wGame (gameDeal .~ Just kyoku) >> sendGameEvents events)

-- | Just execute the action - if it fails the worker dies! Use with care.
unsafeRoundM :: RoundM a -> Worker a
unsafeRoundM = roundM >=> either failed go
  where failed e  = error $ "unsafeRoundM: unexpected Left: " <> unpack e
        go (a, m) = m >> return a

-- | Argument player *must* be present in the game.
gameActionToKyokuInput :: Player -> GameAction -> Worker MachineInput
gameActionToKyokuInput p ga = do
    k <- unsafeRoundM (playerToKaze p)
    return $ case ga of
        GameTurn ta  -> InpTurnAction k ta
        GameShout sh -> InpShout k sh
        GameDontCare -> InpPass k

-- | Safe kyoku step. Checks whether the step is valid, but does not do
-- any relevant modifications to the worker state. The modifications can be
-- done by running the @Worker@ action of the result.
safeStep :: MachineInput -> Worker (Either Text (Machine, Worker ()))
safeStep inp = rview wMachine >>= roundM . flip step inp

-- | See @unsafeRoundM@.
unsafeStep :: MachineInput -> Worker Machine
unsafeStep inp = do
    m <- rview wMachine >>= unsafeRoundM . flip step inp
    rswap wMachine m $> m

-- | Wait until someone inputs a valid action to the kyoku machine
-- (@stepByClient@) or a timeout is reached. After timeout we continue with
-- an @InpAuto@.
stepByClientOrTimeout :: Worker Machine
stepByClientOrTimeout = do
    let secs = 15 -- TODO hard-coded limit
    join $ workerRace secs stepByClient
         $ do $logDebug "Time-out, proceeding automatically."
              unsafeStep InpAuto

-- | Wait indefinetely until someone inputs a valid action to the kyoku
-- machine.
stepByClient :: Worker (Worker Machine)
stepByClient = go
    where go = takeInput >>= processInput >>= maybe go (\(m, a) -> return $ a >> rswap wMachine m $> m)
