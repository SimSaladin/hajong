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
module Hajong.Worker where

import           ClassyPrelude
import           Control.Concurrent
import           Control.Monad.Trans.Either
import           Control.Lens
import           Control.Concurrent.Async
import           Control.Monad.Reader       (runReaderT, ReaderT, MonadReader)
import qualified Data.List as L

----------------------------------------------
import Hajong.Connections
import Hajong.Game

-- Solve turn player instead
-- let Just turnPlayer = gs ^? gameRound._Just.riichiPublic.riichiTurn
-- let (_,mc',_):_     = gs ^. gamePlayers & filter (^._1.to (== turnPlayer))

newtype Worker a = Worker { runWorker :: ReaderT WorkerState IO a }
                   deriving (Functor, Monad, MonadIO, MonadReader WorkerState)

type WorkerState = (TVar (GameState Client), TMVar WorkerInput)

type WCont = Worker ()

data WorkerInput = WorkerAction (Worker ())
                 | WorkerClientPass Client
                 | WorkerClientAction Client TurnAction

-- | Solve Player based on the client.
clientToPlayer :: Client -> GameState Client -> Player
clientToPlayer c gs = pl
    where (pl,_,_):_ = gs ^. gamePlayers & filter (^._2.to (== Just c))

-- | Start the game worker thread
startWorker :: TMVar WorkerInput -> GameState Client -> IO ()
startWorker inVar gs = do
    gsv <- newTVarIO gs
    void $ forkIO $ runReaderT (runWorker workerWaitPlayers) (gsv, inVar)

-- * Primitive

-- | Take the element from input queue (blocking).
workerTake :: Worker WorkerInput
workerTake = liftIO . atomically . takeTMVar =<< view _2

-- | Process events until a valid turn action, when the continuation
-- induced by it is returned.
workerUntilTurnAction :: Worker WCont
workerUntilTurnAction = workerTake >>= workerAction >>= maybe workerUntilTurnAction return

-- | Either apply the action and return Nothing, or return the
-- continuation from applying a WorkerClientAction.
workerAction :: WorkerInput -> Worker (Maybe WCont)
workerAction (WorkerAction m)          = m >> return Nothing
workerAction (WorkerClientPass _)      = return Nothing
workerAction (WorkerClientAction c ta) = workerProcessTurnAction ta c

-- | Attempt to apply the TurnAction. Return the continuation if it
-- succeeded.
workerProcessTurnAction :: TurnAction -> Client -> Worker (Maybe WCont)
workerProcessTurnAction ta c = do
    gs <- liftIO . readTVarIO =<< view _1
    let player = clientToPlayer c gs

    case gsRoundAction (runTurn player ta) gs & _Left %~ ("Game error: " <>) of
        Left err                      -> unicastError c err >> return Nothing
        Right (mhand, secret, events) -> return $ Just $ do
            -- the continuation
            updateState secret events
            case mhand of
                Just hand -> unicast c $ GameHandChanged hand
                Nothing -> return ()
            case ta of
                TurnTileDraw _ _    -> waitForAction c
                TurnAnkan _         -> waitForAction c
                TurnTileDiscard _ _ -> waitForShouts $ _riichiWaitShoutsFrom secret
                TurnShouted _ _     -> advanceTurn
                TurnAuto            -> advanceTurn

-- | The game state has changed.
updateState :: RiichiSecret -> [RoundEvent] -> Worker ()
updateState secret events = do
    gsv <- view _1
    gs  <- atomically $ do
        modifyTVar gsv $ gameRound._Just.riichiSecret .~ secret
        readTVar gsv
    multicast gs (GameEvents events)

-- | "workerRace n ma mb" races between "ma" and "delay n >> mb"
workerRace :: Int -> Worker a -> Worker a -> Worker a
workerRace n a b = do
    s <- view id
    res <- liftIO $ race (runWorker a `runReaderT` s)
                        (threadDelay n >> (runWorker b `runReaderT` s))
    return $ either id id res

workerAddPlayer :: Client -> (GameState Client -> IO ()) -> Worker ()
workerAddPlayer client callback = do
    gsv <- view _1
    e_gs <- atomically $ runEitherT $ do
        gs  <- lift $ readTVar gsv
        gs' <- maybeToEitherT "Game is full" $ gsAddPlayer client gs
        lift $ writeTVar gsv gs'
        return gs'

    clientEither client e_gs (liftIO . callback)

-- * Continuations

-- | Begin the game including the first round when all players have joined.
workerWaitPlayers :: WCont
workerWaitPlayers = do
    gsv <- view _1
    may_begin <- gsMaybeFirstRound <$> liftIO (readTVarIO gsv)
    case may_begin of
        Nothing -> do
            wi <- workerTake
            case wi of
                WorkerAction m -> m >> workerWaitPlayers
                _              -> workerWaitPlayers
        Just m -> do
            gs <- liftIO m
            atomically $ writeTVar gsv gs

            -- send the GamePlayer's
            forM_ (gs^.gamePlayers) $ \(p, Just c, _) ->
                unicast c $ StartGame
                    $ playerPlayers.each._2._Just %~ getNick
                    $ gsPlayerLookup gs p ^?! _Just

            workerTurnBegin

-- | A new turn begins.
workerTurnBegin :: WCont
workerTurnBegin = join workerUntilTurnAction

-- | Advance turn to the next player.
advanceTurn :: WCont
advanceTurn = do
    gs <- atomically . readTVar =<< view _1
    let Right (mres, secret, events) = gsRoundAction advanceAfterDiscard gs
    updateState secret events
    maybe workerTurnBegin handleResults mres

-- | The round ended.
handleResults :: RoundResults -> WCont
handleResults res =
    error "handleResults: TODO"

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
    gs  <- liftIO . readTVarIO =<< view _1
    inv <- view _2
    shv <- liftIO newEmptyTMVarIO

    let waitAll [] = return advanceTurn
        waitAll xs@(x:_) = do
            let passOn p = waitAll (L.delete p xs)
            wi    <- workerTake
            mcont <- workerAction wi
            case (mcont, wi) of
                (_, WorkerClientPass c) -> passOn (clientToPlayer c gs)
                (Just cont, WorkerClientAction c (TurnShouted _ p))
                    | p == x            -> return cont
                    | otherwise         -> atomically (putTMVar shv cont) >> passOn p
                _                       -> waitAll xs

    join $ workerRace 10000000 (waitAll players) $ return
        (atomically (tryTakeTMVar shv) >>= fromMaybe advanceTurn)

-- | Expect an action in the queue, or timeout and automatically end the
-- turn with a default action.
waitForAction :: Client -> WCont
waitForAction c = join $ workerRace 10000000 workerUntilTurnAction $ return $ do
    Just cont <- workerProcessTurnAction TurnAuto c -- TurnAuto never fails
    cont
