{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Worker
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
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
------------------------------------------------------------------------------
module Hajong.Worker
    ( WorkerState(..)
    , WorkerInput(..)
    , startWorker
    ) where

import           Control.Concurrent
import           Control.Monad.Trans.Either
import           Control.Monad.Logger
import           Control.Concurrent.Async
import           Data.Maybe (fromJust)
import           System.Log.FastLogger

--------------------------------------------------------------
import           Hajong.Connections hiding (multicast)
import qualified Hajong.Connections as Con (multicast)
import           Mahjong
default (Text)

-- * Types

--  TODO This could probably use ContT.
newtype Worker a = Worker { runWorker :: LoggingT (ReaderT WorkerState IO) a }
                   deriving ( Functor, Applicative, Monad, MonadIO
                            , MonadLogger, MonadReader WorkerState)

-- type WorkerState = (TVar (GameState Client), TMVar WorkerInput)
data WorkerState = WorkerState
                 { _gameVar  :: TVar (GameState Client)
                 , _inputVar :: TMVar WorkerInput
                 , loggerSet :: LoggerSet
                 }

data WorkerInput = WorkerAddPlayer Client (GameState Client -> IO ())
                 | WorkerPartPlayer Client (GameState Client -> IO ())
                 | WorkerGameAction Client GameAction
                 | WorkerForceStart

type WCont = Worker ()

makeLenses ''WorkerState

-- * Entry points

-- | Fork a new worker thread
startWorker :: TMVar WorkerInput
            -> GameState Client
            -> LoggerSet
            -> IO ThreadId
startWorker input gs logger = do
    gsvar <- newTVarIO gs
    forkIO $ runWCont (WorkerState gsvar input logger) waitPlayersAndBegin

-- * Unwrap monads

runWCont :: WorkerState -> Worker a -> IO a
runWCont st cont = (runWorker cont `runLoggingT` logger) `runReaderT` st
    where
        logger loc src level = pushLogStr (loggerSet st) . defaultLogStr loc src level

roundM :: RoundM' a -> Worker (Either Text (a, Worker ()))
roundM ma = liftM (fmap tores . runRoundM ma) $ rview gameVar
    where
        tores (res, secret, events) =
            (res, updateState secret events >> sendGameEvents events)

unsafeRoundM :: RoundM' a -> Worker (a, Worker ())
unsafeRoundM = roundM >=> either failed return
  where
    failed e = error $ "unsafeRoundM: unexpected Left: " <> unpack e

unsafeRoundM_ :: RoundM' () -> Worker ()
unsafeRoundM_ = join . fmap snd . unsafeRoundM

-- * Update state and emit events

-- | Documentation for 'multicast'
multicast :: Event -> Worker ()
multicast ev = rview gameVar >>= (`Con.multicast` ev)

-- | The game state has changed.
updateState :: RiichiSecret -> [GameEvent] -> Worker ()
updateState secret events = rmodify gameVar $
    gameRound._Just %~
        ( (riichiSecret .~ secret)
        . (riichiPublic %~ applyGameEvents' events) )

-- | Hide sensitive info per player
sendGameEvents :: [GameEvent] -> Worker ()
sendGameEvents events = do
    gs <- rview gameVar
    public <- filterM (f gs) events
    multicast $ InGameEvents public
    where
        f gs e@(RoundPrivateChange p _) = sendPrivate gs p e
        f gs e@(RoundPrivateWaitForShout p _) = sendPrivate gs p e
        f gs e@(RoundPrivateWaitForTurnAction p _) = sendPrivate gs p e
        f gs e@(RoundPrivateStarts pg)  = sendPrivate gs (_playerPlayer pg) e
        f _ _                           = return True

        sendPrivate gs p e =
            maybe (return ()) (`unicast` InGamePrivateEvent e) (playerToClient gs p)
            >> return False

-- * Take input

-- | Take the next element from input queue (blocking).
takeInput :: Worker WorkerInput
takeInput = liftIO . atomically . takeTMVar =<< view inputVar

-- | Read (and apply) input from WorkerInput until a valid TurnAction.
-- Returns the continuation derived from the TurnAction.
takeInputTurnAction :: Worker WCont
takeInputTurnAction = workerAction'
    workerProcessTurnAction (\_ _ -> return Nothing) (\_ -> return Nothing) (return Nothing)
    >>= maybe takeInputTurnAction return

-- * Helper combinators

workerAction' :: (TurnAction -> Client -> Worker a) -- ^ TurnAction
              -> (Client -> Shout -> Worker a) -- ^ Shout
              -> (Client -> Worker a) -- ^ GameDontCare
              -> Worker a -- ^ Action
              -> Worker a
workerAction' f_ta f_sh f_dc f_ac = do
    wi <- takeInput
    case wi of
        WorkerGameAction c ga -> case ga of
            GameTurn ta     -> f_ta ta c
            GameShout shout -> f_sh c shout
            GameDontCare    -> f_dc c
        _ -> runOtherAction wi >> f_ac

runOtherAction :: WorkerInput -> Worker ()
runOtherAction wi = case wi of
    WorkerAddPlayer client callback -> do
        gsv  <- view gameVar

        e_gs <- atomically $ runEitherT $ do
            gs  <- lift $ readTVar gsv
            gs' <- case clientToPlayer client gs of
                Nothing -> addClient client gs ? "Game is full"
                Just p  -> return $ setClient client p gs
            lift $ writeTVar gsv gs'
            return gs'

        clientEither client e_gs $ \gs -> do
            when (gs^.gameRound.to isJust) $
                unsafeRoundM_ $ tellPlayerState $ fromJust $ clientToPlayer client gs
            liftIO $ callback gs

    WorkerPartPlayer client callback -> do
        gsv <- view gameVar

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

    WorkerForceStart     -> rmodify gameVar $ over (gamePlayers.each) (\c -> c { isReady = True })
    WorkerGameAction _ _ -> $logWarn "WorkerGameAction when other was expected"

-- | Attempt to apply the TurnAction. Return the continuation if it
-- succeeded.
workerProcessTurnAction :: TurnAction -> Client -> Worker (Maybe WCont)
workerProcessTurnAction ta c = do
    gs <- rview gameVar

    case clientToPlayer c gs of
        Nothing     -> unicastError c "You are not a player in this game!" >> return Nothing
        Just player -> do
            res <- roundM (runTurn player ta)
            case res of
                Left err      -> unicastError c err >> return Nothing
                Right (_, ma) -> return $ Just $ do
                    ma
                    case ta of
                        TurnTileDraw _ _    -> turnActionOrTimeout
                        TurnAnkan _         -> turnActionOrTimeout
                        TurnTileDiscard _ _ -> waitForShouts

-- | "workerRace n ma b" races between "ma" and "threadDelay n" (return b)
workerRace :: Int -> Worker a -> a -> Worker a
workerRace secs ma b = do
    s   <- view id
    res <- liftIO $ runWCont s ma `race` threadDelay (secs * 1000000)
    either (\a -> $logDebug "Proceed before time out" >> return a)
           (\_ -> return b) res

-- * Game flow continuations

-- | Begin the game including the first round when all players have joined.
waitPlayersAndBegin :: WCont
waitPlayersAndBegin = $logInfo "Waiting for players" >> go
    where
        go = rview gameVar >>= maybe (takeInput >>= runOtherAction >> go)
                                     (liftIO >=> beginRound) . maybeNextRound

beginRound :: GameState Client -> WCont
beginRound gs = do
    void $ rswap gameVar gs
    $logInfo "Round begins"
    unsafeRoundM_ startRound >> unsafeRoundM_ autoDraw
    turnActionOrTimeout

-- | A new turn begins.
--
-- | Expect an action in the queue, or timeout after the n seconds and
-- automatically end the turn with a default action.
turnActionOrTimeout :: WCont
turnActionOrTimeout = do
    -- TODO configurable secs

    unsafeRoundM_ (turnWaiting 30)

    join $ workerRace 30 takeInputTurnAction
        ($logDebug "Time-out, auto-discarding"
            >> unsafeRoundM_ autoDiscard >> waitForShouts)

-- | Advance turn to the next player. After a discard by previous player.
afterDiscard :: WCont
afterDiscard = do
    (r, ma) <- unsafeRoundM advanceAfterDiscard
    ma
    maybe ($logDebug "New turn begins" >> unsafeRoundM_ autoDraw >> turnActionOrTimeout) endRound r

advanceWithShout :: Player -> Shout -> WCont
advanceWithShout pp sh = do
    $logDebug "Advancing with a shout"
    roundM (runShout sh pp) >>= either $logError (go . snd)
    where
        go ma = ma >> turnActionOrTimeout

-- | After a discard, there are three possible branchings:
--
--  * Highest priority shout is called: it is then processed immediately.
--  * At least one of plausible shouts is called AND
--      (all shouts are called OR pass confirmed OR time limit is reached):
--      the highest called shout is then processed.
--  * No shouts plausible OR all plausible shouts are passed on OR time
--      limit reached: advance to next turn.
waitForShouts :: WCont
waitForShouts = do

    gs           <- rview gameVar
    winningShout <- liftIO newEmptyTMVarIO
    (players, _) <- unsafeRoundM getWaitingForShouts -- TODO should include *and* be ordered by shout

    $logDebug ("Waiting for shouts (" <> tshow players <> ") after the discard")

    -- Important invariant of waitAll's first argument:
    --  Shout priorities must be in /decreasing/ order. (This whole thing
    --  is wrong as it takes players and not shouts - TODO)

    let waitAll [] = return afterDiscard
        waitAll xs = workerAction' (\_ _ -> waitAll xs) (shoutHandler xs) (passOn xs) (waitAll xs)

        passOn xs c = waitAll $ maybe id (\p -> filter (^._2.to (== p))) (c `clientToPlayer` gs) xs

        shoutHandler                [] _     _ = error "shoutHandler: empty list (this is a bug)"
        shoutHandler xs@((_, pp) : _ ) c shout = case c `clientToPlayer` gs of
              Just p
                | p == pp   -> return $ p `advanceWithShout` shout -- Highest priority
                | otherwise -> 
                        -- atomically $ putTMVar winningShout $ return $ p `advanceWithShout` shout
                        error "This thing is broken TODO"
                        passOn xs c
              Nothing -> error "Client not found"

    join $ workerRace 20 -- TODO Configurable
        (waitAll players)
        (atomically (tryTakeTMVar winningShout) >>= fromMaybe afterDiscard)

-- | The round ended.
endRound :: RoundResults -> WCont
endRound results = do
    $logInfo $ "Round ended (" ++ tshow results ++ ")"
    maybe endGame (liftIO >=> beginRound) . maybeNextRound =<< rview gameVar

endGame :: WCont
endGame = $logInfo "Game ended, stopping worker."
    -- TODO: Inform the main server process?
