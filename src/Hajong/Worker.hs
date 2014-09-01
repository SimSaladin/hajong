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
    ( WorkerState(..), WorkerInput(..)
    , startWorker
    , workerAddPlayer
    ) where

import           Data.Maybe (fromJust)
import           Control.Concurrent
import           Control.Monad.Trans.Either
import           Control.Monad.Logger
import           Control.Concurrent.Async
import           Control.Monad.Reader       (runReaderT, ReaderT)
import qualified Data.List as L

----------------------------------------------
import Hajong.Connections hiding (multicast)
import qualified Hajong.Connections as Con (multicast)
import Hajong.Game

default (Text)

-- * Types

newtype Worker a = Worker { runWorker :: LoggingT (ReaderT WorkerState IO) a }
                   deriving ( Functor, Applicative, Monad, MonadIO
                            , MonadLogger, MonadReader WorkerState)

-- type WorkerState = (TVar (GameState Client), TMVar WorkerInput)
data WorkerState = WorkerState
                 { _gameVar :: TVar (GameState Client)
                 , _inputVar :: TMVar WorkerInput
                 , _loggerFun :: forall a. LoggingT (ReaderT WorkerState IO) a -> (ReaderT WorkerState IO) a
                    -- TODO supply only a LoggerSet or something, instead
                    -- of unwrapping LoggingT's every time in threads!
                 }

data WorkerInput = WorkerAction (Worker ())
                 | WorkerClientParts Client
                 | WorkerClientAction Client GameAction

type WCont = Worker ()

makeLenses ''WorkerState

-- * Entry points

-- | Fork a new worker thread
startWorker :: (m ~ ReaderT WorkerState IO) => TMVar WorkerInput -> GameState Client -> (forall a. LoggingT m a -> m a) -> IO ThreadId
startWorker input gs logger = do
    gsvar <- newTVarIO gs
    forkIO $ runWCont (WorkerState gsvar input logger) waitPlayersAndBegin

-- ** Injecting to worker

workerAddPlayer :: Client -> (GameState Client -> IO ()) -> Worker ()
workerAddPlayer client callback = do
    gsv <- view gameVar
    e_gs <- atomically $ runEitherT $ do
        gs  <- lift $ readTVar gsv
        gs' <- addClient client gs ? "Game is full" 
        lift $ writeTVar gsv gs'
        return gs'

    clientEither client e_gs (liftIO . callback)

-- * Pure functions

-- | Solve Player based on the client.
clientToPlayer :: Client -> GameState Client -> Player
clientToPlayer c gs = pl
    where (Just (pl,_)) = gs^.gamePlayers & ifind (const (== Just c))

-- * Unwrap monads

runWCont :: WorkerState -> Worker a -> IO a
runWCont st cont = runReaderT (_loggerFun st $ runWorker cont) st

workerRoundM :: RoundM' a -> Worker (Either Text (a, Worker ()))
workerRoundM ma = liftM (fmap tores . runRoundM ma) $ rview gameVar
    where
        tores (res, secret, events) =
            (res, updateState secret events >> sendGameEvents events)

-- * Update state and emit events

-- | Documentation for 'multicast'
multicast :: Event -> Worker ()
multicast ev = rview gameVar >>= (`Con.multicast` ev)

-- | The game state has changed.
updateState :: RiichiSecret -> [GameEvent] -> Worker ()
updateState secret events = rmodify gameVar $
    gameRound._Just %~ ((riichiSecret .~ secret) . (riichiPublic %~ applyGameEvents' events))

-- | Hide sensitive info per player
sendGameEvents :: [GameEvent] -> Worker ()
sendGameEvents events = do
    gs <- rview gameVar
    public <- filterM (f gs) events
    multicast $ InGameEvents public
    where
        f gs e@(RoundPrivateChange p _) = sendPrivate gs p e
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

-- | Combinator to processing a WorkerAction.
workerAction' :: (TurnAction -> Client -> Worker a) -- ^ TurnAction
              -> (Client -> Shout -> Worker a) -- ^ Shout
              -> (Client -> Worker a) -- ^ GameDontCare
              -> Worker a -- ^ Action
              -> Worker a
workerAction' f_ta f_sh f_dc f_ac = do
    wi <- takeInput
    case wi of
        WorkerAction m          -> m >> f_ac
        WorkerClientAction c ga -> case ga of
            GameTurn ta     -> f_ta ta c
            GameShout shout -> f_sh c shout
            GameDontCare    -> f_dc c

-- | Attempt to apply the TurnAction. Return the continuation if it
-- succeeded.
workerProcessTurnAction :: TurnAction -> Client -> Worker (Maybe WCont)
workerProcessTurnAction ta c = do
    gs <- rview gameVar
    let player = clientToPlayer c gs

    res <- workerRoundM (runTurn player ta)
    case res of
        Left err      -> unicastError c err >> return Nothing
        Right (_, ma) -> return $ Just $ do
            ma
            case ta of
                TurnTileDraw _ _    -> turnActionOrTimeout
                TurnAnkan _         -> turnActionOrTimeout
                TurnTileDiscard _ _ -> waitForShouts $
                    gs^?!gameRound._Just^.riichiSecret.riichiWaitShoutsFrom

-- | "workerRace n ma mb" races between "ma" and "delay n >> mb"
workerRace :: Int -> Worker a -> Worker a -> Worker a
workerRace n a b = do
    s <- view id
    res <- liftIO $ runWCont s a `race` threadDelay n --  >> runWCont s b)
    either return (const b) res

-- * Game flow continuations

-- | Begin the game including the first round when all players have joined.
waitPlayersAndBegin :: WCont
waitPlayersAndBegin = logInfoN "Waiting for players" >> go
    where
        go = rview gameVar >>=
             maybe (takeInput >>= process >> go)
                   (liftIO >=> beginRound) . maybeNextRound

        process (WorkerAction m) = m
        process _                = logWarnN "Got ingame action when not in game"

beginRound :: GameState Client -> WCont
beginRound gs = do
    logInfoN "Round begins"
    rswap gameVar gs >> workerRoundM startRound >>= either (const $ error "startRound failed. (The Impossible happened)") snd
    turnActionOrTimeout

-- | A new turn begins.
--
-- | Expect an action in the queue, or timeout and automatically end the
-- turn with a default action.
turnActionOrTimeout :: WCont
turnActionOrTimeout =
    -- TODO notify of timeout to client
    join $ workerRace 10000000 takeInputTurnAction $ return $ do
        gs <- rview gameVar
        let Just tp = gs ^? gameRound._Just.riichiPublic.riichiTurn
            Just c  = gs ^? gamePlayers.at tp._Just._Just
            Just a  = gs ^? gameRound._Just.to advanceAuto
        join $ fromJust <$> workerProcessTurnAction a c

-- | Advance turn to the next player.
advanceTurn :: WCont
advanceTurn = do
    logDebugN "Advancing turn after a discard"
    workerRoundM advanceAfterDiscard >>= either logErrorN go -- TODO Info client too
    where
        go (r, ma) = ma >> maybe (logDebugN "New turn begins" >> turnActionOrTimeout) endRound r

advanceWithShout :: Player -> Shout -> WCont
advanceWithShout p sh = do
    logDebugN "Advancing with a shout"
    workerRoundM (runShout sh p) >>= either logErrorN (go . snd)
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
waitForShouts :: [Player] -> WCont
waitForShouts players = do
    logDebugN "Waiting for shouts after the discard"
    gs  <- rview gameVar
    shv <- liftIO newEmptyTMVarIO

    let waitAll       [] = return advanceTurn
        waitAll xs@(x:_) = workerAction' (\_ _ -> waitAll xs) shoutHandler passOn (waitAll xs)
                where
            passOn c = waitAll (L.delete (clientToPlayer c gs) xs)
            shoutHandler c sh
                | shoutedFrom sh == x = return $ advanceWithShout (clientToPlayer c gs) sh
                                                 -- Highest priority ^
                | otherwise           = do
                    atomically (putTMVar shv (advanceWithShout (clientToPlayer c gs) sh))
                    passOn c

    join $ workerRace 10000000
        (waitAll players)
        (return $ atomically (tryTakeTMVar shv) >>= fromMaybe advanceTurn)

-- | The round ended.
endRound :: RoundResults -> WCont
endRound results = do
    logInfoN $ "Round ended (" ++ tshow results ++ ")"
    maybe endGame (liftIO >=> beginRound) . maybeNextRound =<< rview gameVar

endGame :: WCont
endGame = logInfoN "Game ended, stopping worker."
    -- TODO: Inform the main server process?
