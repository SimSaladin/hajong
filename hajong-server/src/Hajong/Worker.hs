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
    ( WorkerData(..), wGame, wSettings, wInput, wLogger
    , WorkerInput(..), WorkerResult
    , startWorker
    ) where

import           Hajong.Connections
import           Hajong.Client
import           Mahjong
------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad.Logger
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
type WorkerResult = Either SomeException FinalPoints
                    -- ^ Left only on an unexpected event, a bug.

data WorkerInput = WorkerAddPlayer Client (GameState Client -> IO ())
                 | WorkerPartPlayer Client (GameState Client -> IO ())
                 | WorkerGameAction Client GameAction
                 | WorkerForceStart

newtype Worker a = Worker { runWorker :: LoggingT (ReaderT WorkerData IO) a }
                   deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader WorkerData)

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

roundM :: RoundM a -> Worker (Either Text (a, Worker ()))
roundM ma = do
        gs <- rview wGame
        return $ do (res, kyoku, events) <- runRoundM ma gs
                    return (res, rmodify wGame (gameDeal .~ Just kyoku) >> sendGameEvents events)

unsafeRoundM :: RoundM a -> Worker a
unsafeRoundM = roundM >=> either failed go
  where failed e  = error $ "unsafeRoundM: unexpected Left: " <> unpack e
        go (a, m) = m >> return a

-- * Update state and emit events

-- | Send to everyone in game.
multicast :: Event -> Worker ()
multicast ev = do gs <- rview wGame
                  mapM_ (`unicast` ev) (gs^.gamePlayers^..each)

-- | Hide sensitive info per player
sendGameEvents :: [GameEvent] -> Worker ()
sendGameEvents events = do
    gs <- rview wGame
    public <- filterM (f gs) events
    multicast $ InGameEvents public
  where
    f gs e@(DealStarts p _ _)                = sendPrivate gs p e
    f gs e@(DealWaitForShout (p,_,_,_))      = sendPrivate gs p e
    f gs e@(DealWaitForTurnAction (p,_,_,_)) = sendPrivate gs p e
    f gs e@(DealPrivateHandChanged p _ _)    = sendPrivate gs p e
    f _ _                                    = return True

    sendPrivate gs p e = do
        maybe (return ()) (`unicast` InGamePrivateEvent e) (playerToClient gs p)
        return False

-- * Input layer

-- | Take the next element from input queue (blocking).
takeInput :: Worker WorkerInput
takeInput = liftIO . atomically . takeTMVar =<< view wInput

-- | Process input. If a WorkerGameAction would succeed, return
-- corresponding MachineInput.
processInput :: WorkerInput -> Worker (Maybe (Machine, Worker ()))
processInput (WorkerAddPlayer client callback)  = addPlayer client callback >> return Nothing
processInput (WorkerPartPlayer client callback) = partPlayer client callback >> return Nothing
processInput WorkerForceStart                   = rmodify wGame (over (gamePlayers.each) (\c -> c { isReady = True })) >> return Nothing
processInput (WorkerGameAction c ga)            = do
        gs <- rview wGame
        case clientToPlayer c gs of
            Nothing -> unicastError c "You are not playing in this game" >> return Nothing
            Just p  -> do
                k  <- unsafeRoundM $ playerToKaze p
                let inp = case ga of
                        GameTurn ta -> InpTurnAction k ta
                        GameShout sh -> InpShout k sh
                        GameDontCare -> InpPass k
                res <- safeStep inp
                case res of
                    Left err                           -> unicastError c err >> return Nothing
                    -- TODO Correct here would be to see if m and previous machine
                    -- constructors match; if they do, apply and return Nothing.
                    -- otherwise yield (m,a)
                    Right (m, a) | GameDontCare <- ga -> a >> rswap wMachine m >> return Nothing -- XXX: This transaction, a >> rswap,  is possibly racy
                                 | otherwise          -> return $ Just (m, a)

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
                               (liftIO >=> beginDeal) . maybeBeginGame

beginDeal :: GameState Client -> WCont
beginDeal gs = do
    void $ rswap wGame gs
    $logInfo "Kyoku starts"
    processMachine =<< rview wMachine

-- | Monitor the kyoku machine and perhaps do something in there.
processMachine :: Machine -> WCont
processMachine NotBegun                       = unsafeStep InpAuto    >>= processMachine -- start
processMachine CheckEndConditionsAfterDiscard = unsafeStep InpAuto    >>= processMachine
processMachine WaitingDraw{}                  = unsafeStep InpAuto    >>= processMachine -- auto draw
processMachine WaitingDiscard{}               = stepByClientOrTimeout >>= processMachine
processMachine WaitingShouts{}                = stepByClientOrTimeout >>= processMachine
processMachine (KyokuEnded res)               = processKyokuEnded res

processKyokuEnded :: KyokuResults -> WCont
processKyokuEnded results = do
    $logInfo $ "Kyoku ended: " <> tshow results
    rmodify wGame (gameDeal._Just.pResults.~Just results)
    Just k <- rview wGame <&> _gameDeal
    liftIO (maybeNextDeal k) >>= either return go -- returns finalpoints or starts next kyoku
  where
    go k = do rmodify wGame (gameDeal.~Just k)
              rmodify wMachine (const NotBegun)
              processMachine NotBegun

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
                writeTVar gsv gs'
                return (Just gs')
            Nothing ->
                return Nothing

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
            let p = fromJust $ clientToPlayer client gs
            unsafeRoundM $ updatePlayerNick p (getNick client)
            unsafeRoundM $ tellPlayerState p
        liftIO $ callback gs

-- * Helper combinators

-- | "workerRace n ma b" races between actions "ma" and "threadDelay n >> return b".
workerRace :: Int -> Worker a -> a -> Worker a
workerRace secs ma b = do
    s   <- view id
    res <- liftIO $ runWCont s ma `race` threadDelay (secs * 1000000)
    either return (\_ -> return b) res

-- | Safe kyoku step. Checks whether the step is valid, but does not do
-- any relevant modifications to the worker state. The modifications can be
-- done by running the @Worker@ action of the result.
safeStep :: MachineInput -> Worker (Either Text (Machine, Worker ()))
safeStep inp = do
    m  <- rview wMachine
    roundM (step m inp)

-- | Takes an unsafe step in the kyoku machine.
-- Use with care, it may crash the worker on a logic bug!
unsafeStep :: MachineInput -> Worker Machine
unsafeStep inp = do
    m  <- rview wMachine
    m' <- unsafeRoundM (step m inp)
    _  <- rswap wMachine m'
    return m'

-- | Wait until someone inputs a valid action to the kyoku machine
-- (@stepByClient@) or a timeout is reached. After timeout we continue with
-- an @InpAuto@.
stepByClientOrTimeout :: Worker Machine
stepByClientOrTimeout = do
    let secs = 15
    join $ workerRace secs stepByClient
         $ do $logDebug "Time-out, proceeding automatically."
              unsafeStep InpAuto

-- | Wait indefinetely until someone inputs a valid action to the kyoku
-- machine.
stepByClient :: Worker (Worker Machine)
stepByClient = go
    where go = do inp <- takeInput
                  res <- processInput inp
                  case res of
                      Nothing -> go
                      Just (m, a) -> return $ a >> rswap wMachine m >> return m
