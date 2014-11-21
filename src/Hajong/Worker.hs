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
------------------------------------------------------------------------------
module Hajong.Worker
    ( WorkerData(..), wGame, wSettings, wInput, wLogger
    , WorkerInput(..)
    , startWorker
    ) where

import           Control.Concurrent
import           Control.Monad.Trans.Either
import           Control.Monad.Logger
import           Control.Concurrent.Async
import           Data.Maybe (fromJust)
import           System.Log.FastLogger
import qualified Data.List as L

--------------------------------------------------------------
import           Hajong.Connections hiding (multicast)
import qualified Hajong.Connections as Con (multicast)
import           Mahjong
default (Text)

-- * Types

-- type WorkerState = (TVar (GameState Client), TMVar WorkerInput)
data WorkerData = WorkerData
                { _wSettings  :: GameSettings
                , _wGame      :: TVar (GameState Client)
                , _wInput     :: TMVar WorkerInput
                , _wLogger    :: LoggerSet
                } deriving (Typeable)

data WorkerInput = WorkerAddPlayer Client (GameState Client -> IO ())
                 | WorkerPartPlayer Client (GameState Client -> IO ())
                 | WorkerGameAction Client GameAction
                 | WorkerForceStart

--  TODO This could probably use ContT.
newtype Worker a = Worker { runWorker :: LoggingT (ReaderT WorkerData IO) a }
                   deriving ( Functor, Applicative, Monad, MonadIO
                            , MonadLogger, MonadReader WorkerData)

type WCont = Worker ()

-- * Lenses

--
makeLenses ''WorkerData

-- * Entry points

-- | Fork a new worker thread
startWorker :: WorkerData -> IO ThreadId
startWorker wdata =
    forkIO $ runWCont wdata $ do
        $logInfo "New Worker started"
        waitPlayersAndBegin
        $logInfo "Worker has finished"

-- * Unwrap monads

runWCont :: WorkerData -> Worker a -> IO a
runWCont st cont = (runWorker cont `runLoggingT` logger) `runReaderT` st
    where
        logger loc src level = pushLogStr (st^.wLogger) . defaultLogStr loc src level

roundM :: RoundM' a -> Worker (Either Text (a, Worker ()))
roundM ma = liftM (fmap tores . runRoundM ma) $ rview wGame
    where
        tores (res, secret, events) =
            (res, updateState secret events >> sendGameEvents events)

unsafeRoundM :: RoundM' a -> Worker a
unsafeRoundM = roundM >=> either failed go
  where
    failed e = error $ "unsafeRoundM: unexpected Left: " <> unpack e
    go (a, m) = m >> return a

-- * Update state and emit events

-- | Documentation for 'multicast'
multicast :: Event -> Worker ()
multicast ev = rview wGame >>= (`Con.multicast` ev)

-- | The game state has changed.
updateState :: RiichiSecret -> [GameEvent] -> Worker ()
updateState secret events = rmodify wGame $
    gameRound._Just %~
        ( (riichiSecret .~ secret)
        . (riichiPublic %~ applyGameEvents' events) )

-- | Hide sensitive info per player
sendGameEvents :: [GameEvent] -> Worker ()
sendGameEvents events = do
    gs <- rview wGame
    public <- filterM (f gs) events
    multicast $ InGameEvents public
    where
        f gs e@(RoundPrivateChange p _) = sendPrivate gs p e
        f gs e@(RoundPrivateWaitForShout p _ _) = sendPrivate gs p e
        f gs e@(RoundPrivateWaitForTurnAction p _) = sendPrivate gs p e
        f gs e@(RoundPrivateStarts pg)  = sendPrivate gs (_playerPlayer pg) e
        f _ _                           = return True

        sendPrivate gs p e = do
            maybe (return ()) (`unicast` InGamePrivateEvent e) (playerToClient gs p)
            return False

-- * Take input

-- | Take the next element from input queue (blocking).
takeInput :: Worker WorkerInput
takeInput = liftIO . atomically . takeTMVar =<< view wInput

-- | Read (and apply) input from WorkerInput until a valid TurnAction.
-- Returns the continuation derived from the TurnAction.
takeInputTurnAction :: Worker WCont
takeInputTurnAction = withEvent
    workerProcessTurnAction
    (\c _ -> notExpected "Someones turn is in progress, you can shout only after a discard" c)
    (notExpected "Someones turn is currently in progress, you ought to care about that")
    (return Nothing)
    >>= maybe takeInputTurnAction return
  where
    notExpected txt c = unicastError c txt >> return Nothing

-- * Helper combinators

-- | Takes an event (running all other actions before one) and applies the
-- relevant callback.
withEvent :: (TurnAction -> Client -> Worker a) -- ^ TurnAction
          -> (Client -> Shout -> Worker a) -- ^ Shout
          -> (Client -> Worker a) -- ^ GameDontCare
          -> Worker a -- ^ Action
          -> Worker a
withEvent f_ta f_sh f_dc f_ac = do
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
        gsv  <- view wGame

        e_gs <- atomically $ runEitherT $ do
            gs  <- lift $ readTVar gsv
            gs' <- case clientToPlayer client gs of
                Nothing -> addClient client gs ? "Game is full"
                Just p  -> return $ setClient client p gs
            lift $ writeTVar gsv gs'
            return gs'

        clientEither client e_gs $ \gs -> do
            when (gs^.gameRound.to isJust) $ do
                let p = fromJust $ clientToPlayer client gs
                unsafeRoundM $ updatePlayerNick p (getNick client)
                unsafeRoundM $ tellPlayerState p
            liftIO $ callback gs

    WorkerPartPlayer client callback -> do
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

    WorkerForceStart     -> rmodify wGame $ over (gamePlayers.each) (\c -> c { isReady = True })
    WorkerGameAction _ _ -> $logWarn "WorkerGameAction when other was expected"

-- | Attempt to apply the TurnAction. Return the continuation if it
-- succeeded.
workerProcessTurnAction :: TurnAction -> Client -> Worker (Maybe WCont)
workerProcessTurnAction ta c = do
    gs <- rview wGame

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
    either return (\_ -> return b) res

-- * Game flow continuations

-- ** Begin game

-- | Begin the game including the first round when all players have joined.
waitPlayersAndBegin :: WCont
waitPlayersAndBegin = $logInfo "Waiting for players" >> go
    where
        go = rview wGame >>= maybe (takeInput >>= runOtherAction >> go)
                                     (liftIO >=> beginRound) . maybeNextRound

beginRound :: GameState Client -> WCont
beginRound gs = do
    void $ rswap wGame gs
    $logInfo "Round begins"
    unsafeRoundM startRound >> unsafeRoundM autoDraw
    turnActionOrTimeout

-- ** Middle game

-- | A new turn begins.
--
-- | Expect an action in the queue, or timeout after the n seconds and
-- automatically end the turn with a default action.
turnActionOrTimeout :: WCont
turnActionOrTimeout = do
    -- TODO configurable secs

    unsafeRoundM (turnWaiting 30)

    join $ workerRace 30 takeInputTurnAction
        ($logDebug "Time-out, auto-discarding"
            >> unsafeRoundM autoDiscard >> waitForShouts)

-- | Advance turn to the next player. After a discard by previous player.
afterDiscard :: WCont
afterDiscard = unsafeRoundM advanceAfterDiscard
    >>= maybe (unsafeRoundM autoDraw >> turnActionOrTimeout) endRound

afterShout :: Player -> Shout -> WCont
afterShout pp sh = do
    $logDebug $ "Advancing with shout " ++ tshow sh
    unsafeRoundM (advanceWithShout sh pp)
    turnActionOrTimeout

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

    gs        <- rview wGame
    win_var   <- liftIO newEmptyTMVarIO -- :: (Player, Kaze, Shout)
    shoutable <- unsafeRoundM getWaitingForShouts

    unless (null shoutable) $
        $logDebug ("Waiting for shouts " <> tshow shoutable <> " after the discard")

    -- Important invariant of waitAll's first argument: Shout priorities
    -- must be in /decreasing/ order.
    --
    -- Also, shoutHandler below is rather wrong and ambiguous due to rule
    -- set differences in cases of multiple ron "at the same time". There
    -- should be a configurable threshold to others call overriding call or
    -- another ron. -- TODO

    let waitAll [] = return afterDiscard
        waitAll xs = withEvent (\_ _ -> waitAll xs) (shoutHandler xs) (passOn xs) (waitAll xs)

        passOn xs c = case c `clientToPlayer` gs of
            Nothing -> unicastError c "You are not playing in this game" >> waitAll xs
            Just p  -> do
                unicast c (InGamePrivateEvent $ RoundPrivateWaitForShout p 0 [])
                waitAll $ filter (^._1.to (/= p)) xs

        shoutHandler xs c s = do
            mv <- atomically $ tryReadTMVar win_var
            case c `clientToPlayer` gs of
                Nothing     -> unicastError c "You are not playing in this game" >> waitAll xs
                Just p      -> case L.findIndex (\(p',_,s') -> (p',s') == (p,s)) xs of
                    Nothing -> unicastError c "Incorrect shout" >> waitAll xs
                    Just i  -> do
                        let shout@(_,k,_) = xs L.!! i
                        case mv of
                            Nothing
                                | i == 0    -> return $ p `afterShout` s
                                | otherwise -> atomically (putTMVar win_var shout) >> waitAll (take i xs)
                            Just (_,k',s') -> case shoutPrecedence undefined (k, s) (k', s') of -- TODO shoutPrecedence here not correct
                                GT | i == 0    -> return $ p `afterShout` s
                                   | otherwise -> atomically (putTMVar win_var shout) >> waitAll (take i xs)
                                EQ -> $logError "Multiple shouts (ron) not yet implemented" >> waitAll xs
                                LT -> waitAll xs

    -- TODO Configurable timeout
    join $ workerRace 20 (waitAll shoutable)
         $ atomically (tryTakeTMVar win_var) >>= \case
            Nothing      -> do $logDebug "Continue without shouts after timeout"
                               afterDiscard
            Just (p,_,s) -> do $logDebug "Continue with non-top priority shout after timeout"
                               afterShout p s

-- ** End game

-- | The round ended.
endRound :: RoundResults -> WCont
endRound results = do
    $logInfo $ "Round ended (" ++ tshow results ++ ")"
    maybe endGame (liftIO >=> beginRound) . maybeNextRound =<< rview wGame

endGame :: WCont
endGame = $logInfo "Game ended, stopping worker."
    -- TODO: Inform the main server process?
