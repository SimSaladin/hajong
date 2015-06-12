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
    , WorkerInput(..), WorkerResult
    , startWorker
    ) where

------------------------------------------------------------------------------
import           Hajong.Connections
import           Hajong.Client
import           Mahjong

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad.Logger
import           Control.Concurrent.Async
import           Data.Maybe (fromJust)
import           System.Log.FastLogger
import qualified Data.List as L

------------------------------------------------------------------------------
default (Text)

-- * Worker

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

--  TODO This could probably use ContT.
newtype Worker a = Worker { runWorker :: LoggingT (ReaderT WorkerData IO) a }
                   deriving ( Functor, Applicative, Monad, MonadIO
                            , MonadLogger, MonadReader WorkerData)

type WCont = Worker FinalPoints -- TODO Include the whole game state

-- ** Lenses

--
makeLenses ''WorkerData

-- * Entry points

-- | Fork a new worker thread
startWorker :: (Either SomeException FinalPoints -> IO ()) -- ^ Finalization
            -> WorkerData -> IO ThreadId
startWorker ends wdata = runWCont wdata waitPlayersAndBegin `forkFinally` ends

-- * Unwrap monads

-- | Run Worker
runWCont :: WorkerData -> Worker a -> IO a
runWCont st cont = (runWorker cont `runLoggingT` logger) `runReaderT` st
    where logger loc src level = pushLogStr (st^.wLogger) . defaultLogStr loc src level

roundM :: RoundM a -> Worker (Either Text (a, Worker ()))
roundM ma = liftM (fmap tores . runRoundM ma) $ rview wGame
    where tores (res, secret, events) =
            (res, updateState secret events >> sendGameEvents events)

unsafeRoundM :: RoundM a -> Worker a
unsafeRoundM = roundM >=> either failed go
  where failed e  = error $ "unsafeRoundM: unexpected Left: " <> unpack e
        go (a, m) = m >> return a

------------------------------------------------------------------------------

-- * Update state and emit events

-- | Send to everyone in game.
multicast :: Event -> Worker ()
multicast ev = do gs <- rview wGame
                  mapM_ (`unicast` ev) (gs^.gamePlayers^..each)

-- | The game state has changed.
updateState :: Kyoku -> [GameEvent] -> Worker ()
updateState deal events = rmodify wGame $
    gameDeal._Just .~ foldl' (flip dealGameEvent) deal events

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

------------------------------------------------------------------------------

-- * Take input

-- | Take the next element from input queue (blocking).
takeInput :: Worker WorkerInput
takeInput = liftIO . atomically . takeTMVar =<< view wInput

------------------------------------------------------------------------------

-- * Game play

safeStep :: MachineInput -> Worker (Either Text (Machine, Worker ()))
safeStep inp = do
    m  <- rview wMachine
    roundM (step m inp)

unsafeStep :: MachineInput -> Worker Machine
unsafeStep inp = do
    m <- rview wMachine
    unsafeRoundM (step m inp) >>= rswap wMachine
    return m

-- | Begin the game including the first round when all players have joined.
waitPlayersAndBegin :: WCont
waitPlayersAndBegin = $logInfo "Waiting for players" >> go
  where
    go = rview wGame >>= maybe (takeInput >>= processInput >> go)
                               (liftIO >=> beginDeal) . maybeBeginGame

beginDeal :: GameState Client -> WCont
beginDeal gs = do
    void $ rswap wGame gs
    $logInfo "Round begins"
    processMachine =<< rview wMachine

processMachine :: Machine -> WCont
processMachine NotBegun                       = unsafeStep InpAuto    >>= processMachine -- start
processMachine CheckEndConditionsAfterDiscard = unsafeStep InpAuto    >>= processMachine
processMachine (WaitingDraw _ _)              = unsafeStep InpAuto    >>= processMachine -- auto draw
processMachine (WaitingDiscard _)             = stepByClientOrTimeout >>= processMachine

stepByClientOrTimeout :: Worker Machine
stepByClientOrTimeout = do
    let secs = 15
    join $ workerRace secs stepByClient
         $ do $logDebug "Time-out, proceeding automatically."
              unsafeStep InpAuto

stepByClient :: Worker (Worker Machine)
stepByClient = go
    where go = do inp <- takeInput
                  res <- processInput inp
                  case res of
                      Nothing -> go
                      Just (m, a) -> return $ a >> rswap wMachine m >> return m

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

-- | 
partPlayer client callback = do
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

-- | 
addPlayer client callback = do
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

-- | "workerRace n ma b" races between "ma" and "threadDelay n" (return b)
workerRace :: Int -> Worker a -> a -> Worker a
workerRace secs ma b = do
    s   <- view id
    res <- liftIO $ runWCont s ma `race` threadDelay (secs * 1000000)
    either return (\_ -> return b) res

-- ????????
