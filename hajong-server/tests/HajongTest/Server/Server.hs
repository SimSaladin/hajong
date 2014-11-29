------------------------------------------------------------------------------
-- | 
-- Module         : HajongTest.Server.Server
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module HajongTest.Server.Server (tests, setupProcess) where

import Control.Concurrent (threadDelay)
import System.IO.Silently
import System.Posix

import Hajong.Server

tests :: TestTree
tests = testGroup "The websocket server"
    [
    ]

-- | Start a server process silenced
setupProcess :: IO ProcessID
setupProcess = do
    pid <- forkProcess $ hSilence [stdout, stderr] $ newServer >>= runServer
    threadDelay 1000000
    return pid