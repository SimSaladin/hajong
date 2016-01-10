{-# LANGUAGE TupleSections #-}
------------------------------------------------------------------------------
-- |
-- Module         : HajongTest.Server.Worker
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module HajongTest.Server.Worker (tests) where

import Control.Concurrent
import System.Log.FastLogger
import System.IO.Unsafe

import Mahjong
import Hajong.Client
import Hajong.Connections
import Hajong.Worker


lgr = unsafePerformIO $ newFileLoggerSet defaultBufSize "test-worker.log"

tests :: TestTree
tests = testGroup "Worker" []
--     [ testProperty "Randomly shooting events to a worker" $ \evs ->
--         ioProperty $ do
--             (f:_, _, tid) <- worker lgr
--             forM_ (evs :: [Event]) f
--             killThread tid
--             return True
--     ]
--
-- -- * Helper framework
--
-- type TestClient = (Client, Event -> IO (), IO Event)
--
-- worker :: LoggerSet -> IO ([Event -> IO ()], TChan (Nick, Event), ThreadId)
-- worker lgr = do
--     wiv <- newEmptyTMVarIO
--     tid <- startWorker wiv (newEmptyGS (dummyClient "dummy") "test-state") lgr
--
--     clients <- testClients
--     clientsEvents <- clientsCombineOutput clients
--     fs  <- forM clients $ \(c, f, _) -> do
--         liftIO $ atomically $ putTMVar wiv $ WorkerAddPlayer c (\_ -> return ())
--         return f
--     return (fs, clientsEvents, tid)
--
-- clientsCombineOutput :: [TestClient] -> IO (TChan (Nick, Event))
-- clientsCombineOutput xs = do
--     chan <- newTChanIO
--     forM_ xs $ \(c, _, m) ->
--         let go = m >>= liftIO . atomically . writeTChan chan . (getNick c,) >> go
--             in void $ forkIO go
--     return chan
--
-- testClients :: IO [TestClient]
-- testClients = zipWith3 testClient ["1", "2", "3", "4"]
--     <$> replicateM 4 newEmptyTMVarIO
--     <*> replicateM 4 newTChanIO
--
-- testClient :: Nick -> TMVar Event -> TChan Event -> TestClient
-- testClient nick input_v output_v =
--     ( Client
--         { getNick = nick
--         , isReal  = True
--         , isReady = True
--         , unicast = liftIO . atomically . writeTChan output_v
--         , receive = liftIO . atomically $ takeTMVar input_v
--         }
--     , void . atomically . tryPutTMVar input_v
--     , atomically $ readTChan output_v
--     )
