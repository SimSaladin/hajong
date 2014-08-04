{-# LANGUAGE TupleSections #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Worker
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Worker where

import Control.Concurrent (forkIO)
import Hajong.Game
import Hajong.Connections
import Hajong.Worker

workerTests :: TestTree
workerTests = testGroup "Worker"
    [ testProperty "Randomly shooting events to worker" $ \evs ->
        ioProperty $ do
            (f:_, tc) <- worker
            forM_ (evs :: [Event]) f
            return True
    ]

-- * Helper framework

worker :: IO ([Event -> IO ()], TChan (Nick, Event))
worker = do
    wiv <- newTestWorker
    cs  <- testClients
    out <- clientsCombineOutput cs
    fs  <- forM cs $ \(c, f, _) -> do
        liftIO $ atomically $ putTMVar wiv $ WorkerAction $ workerAddPlayer c (\_ -> return ())
        return f
    return (fs, out)

newTestWorker :: IO (TMVar WorkerInput)
newTestWorker = do
    wiv <- newEmptyTMVarIO
    newEmptyGS wiv (newGameState "test-state")
    return wiv

clientsCombineOutput :: [(Client, Event -> IO (), IO Event)] -> IO (TChan (Nick, Event))
clientsCombineOutput xs = do
    chan <- newTChanIO
    forM_ xs $ \(c, _, m) ->
        let go = m >>= liftIO . atomically . writeTChan chan . (getNick c,) >> go
            in void $ forkIO go
    return chan

testClients :: IO [(Client, Event -> IO (), IO Event)]
testClients = do
    wiv <- newTestWorker
    inputs <- replicateM 4 newEmptyTMVarIO
    outputs <- replicateM 4 newTChanIO
    return $ zipWith3 testClient ["1", "2", "3", "4"]
                        inputs outputs

testClient :: Nick -> TMVar Event -> TChan Event -> (Client, Event -> IO (), IO Event)
testClient nick input_v output_v =
    (Client nick
        (liftIO . atomically . writeTChan output_v)
        (liftIO . atomically $ takeTMVar input_v)
    , emit, receive)
    where
        emit = void . atomically . tryPutTMVar input_v
        receive = atomically $ readTChan output_v
