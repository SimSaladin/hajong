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
module Hajong.Worker where

import           Data.Maybe (fromJust)
import           Control.Concurrent
import           Control.Monad.Trans.Either
import           Control.Concurrent.Async
import           Control.Monad.Reader       (runReaderT, ReaderT)
import qualified Data.List as L

----------------------------------------------
import Hajong.Connections
import Hajong.Game
default (Text)

newtype Worker a = Worker { runWorker :: ReaderT WorkerState IO a }
                   deriving (Functor, Applicative, Monad, MonadIO, MonadReader WorkerState)

type WorkerState = (TVar (GameState Client), TMVar WorkerInput)

type WCont = Worker ()

data WorkerInput = WorkerAction (Worker ())
                 | WorkerClientAction Client GameAction

-- | Solve Player based on the client.
clientToPlayer :: Client -> GameState Client -> Player
clientToPlayer c gs = pl
    where (Just (pl,_)) = gs^.gamePlayers & ifind (const (== Just c))

-- | Start the game worker thread
startWorker :: TMVar WorkerInput -> GameState Client -> IO ThreadId
startWorker inVar gs = do
    print "Worker started"
    gsv <- newTVarIO gs
    forkIO $ do
        runReaderT (runWorker waitingPlayers) (gsv, inVar)
        print "Worker has been stopped"
        -- TODO inform main thread, etc.

-- * Primitive

-- | Take the element from input queue (blocking).
workerTake :: Worker WorkerInput
workerTake = liftIO . atomically . takeTMVar =<< view _2

workerAction' :: (TurnAction -> Client -> Worker a) -- ^ TurnAction
              -> (Client -> Shout -> Worker a) -- ^ Shout
              -> (Client -> Worker a) -- ^ GameDontCare
              -> Worker a -- ^ Action
              -> Worker a
workerAction' f_ta f_sh f_dc f_ac = do
    wi <- workerTake
    case wi of
        WorkerAction m          -> m >> f_ac
        WorkerClientAction c ga -> case ga of
            GameTurn ta     -> f_ta ta c
            GameShout shout -> f_sh c shout
            GameDontCare    -> f_dc c

-- | Either apply the action and return Nothing, or return the
-- continuation from applying a WorkerClientAction.
workerTakeTurnAction :: Worker WCont
workerTakeTurnAction = workerAction'
    workerProcessTurnAction (\_ _ -> return Nothing) (\_ -> return Nothing) (return Nothing)
    >>= maybe workerTakeTurnAction return

-- | Attempt to apply the TurnAction. Return the continuation if it
-- succeeded.
workerProcessTurnAction :: TurnAction -> Client -> Worker (Maybe WCont)
workerProcessTurnAction ta c = do
    gs <- rview _1
    let player = clientToPlayer c gs

    res <- workerRoundM (runTurn player ta)
    case res of
        Left err      -> unicastError c err >> return Nothing
        Right (_, ma) -> return $ Just $ do
            ma
            case ta of
                TurnTileDraw _ _    -> workerWaitTurnAction
                TurnAnkan _         -> workerWaitTurnAction
                TurnTileDiscard _ _ -> waitForShouts $
                    gs^?!gameRound._Just^.riichiSecret.riichiWaitShoutsFrom

workerRoundM :: RoundM' a -> Worker (Either Text (a, Worker ()))
workerRoundM ma = liftM (fmap tores . runRoundM ma) $ atomically . readTVar =<< view _1
    where tores (res, secret, events) = (res, updateState secret events >> sendGameEvents events)

-- | The game state has changed.
updateState :: RiichiSecret -> [GameEvent] -> Worker ()
updateState secret events = rmodify _1 $
    gameRound._Just %~ ((riichiSecret .~ secret) . (riichiPublic %~ applyGameEvents' events))

-- | Hide sensitive info per player
sendGameEvents :: [GameEvent] -> Worker ()
sendGameEvents events = do
    gs <- view _1 >>= atomically . readTVar
    public <- filterM (f gs) events
    multicast gs $ InGameEvents public
    where
        f gs e@(RoundPrivateChange p _) = sendPrivate gs p e
        f gs e@(RoundPrivateStarts pg)  = sendPrivate gs (_playerPlayer pg) e
        f _ _                           = return True

        sendPrivate gs p e =
            maybe (return ()) (`unicast` InGamePrivateEvent e) (playerToClient gs p)
            >> return False

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
        gs' <- maybeToEitherT "Game is full" $ addClient client gs
        lift $ writeTVar gsv gs'
        return gs'

    clientEither client e_gs (liftIO . callback)

-- * Continuations

-- | Begin the game including the first round when all players have joined.
waitingPlayers :: WCont
waitingPlayers = do
    print "Waiting players to join"
    gsv <- view _1
    may_begin <- maybeNextRound <$> liftIO (readTVarIO gsv)
    case may_begin of
        Nothing -> do
            wi <- workerTake
            case wi of
                WorkerAction m -> m >> waitingPlayers
                _              -> waitingPlayers
        Just m -> liftIO m >>= roundBegins

roundBegins :: GameState Client -> WCont
roundBegins gs = do
    print "Round begins"
    _ <- rswap _1 gs
    workerRoundM startRound >>= either print snd
    workerWaitTurnAction

-- | A new turn begins.
--
-- | Expect an action in the queue, or timeout and automatically end the
-- turn with a default action.
workerWaitTurnAction :: WCont
workerWaitTurnAction = do

    join $ workerRace 10000000 workerTakeTurnAction $ return $ do

        gs <- rview _1
        let Just tp = gs ^? gameRound._Just.riichiPublic.riichiTurn
            Just c  = gs ^? gamePlayers.at tp._Just._Just
            Just a  = gs ^? gameRound._Just.to advanceAuto

        join $ fromJust <$> workerProcessTurnAction a c

-- | Advance turn to the next player.
advanceTurn :: WCont
advanceTurn = do
    print "Advancing turn after a discard"
    workerRoundM advanceAfterDiscard >>= either print go
    where go (r, ma) = ma >> maybe (print "New turn begins" >> workerWaitTurnAction) roundEnds r

advanceWithShout :: Player -> Shout -> WCont
advanceWithShout p sh = do
    print "Advancing with a shout"
    workerRoundM (runShout sh p) >>= either print (go . snd)
    where go ma = ma >> workerWaitTurnAction

-- | The round ended.
roundEnds :: RoundResults -> WCont
roundEnds res = do
    print "Round ended:"
    print res
    maybe (return ()) (liftIO >=> roundBegins) . maybeNextRound =<< rview _1

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
    print "Waiting for shouts after the discard"
    gs  <- rview _1
    shv <- liftIO newEmptyTMVarIO
    liftIO $ putStrLn "Now waiting for shouts."

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
