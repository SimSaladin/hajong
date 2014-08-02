------------------------------------------------------------------------------
-- | 
-- Module         : Server
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Server (serverTests, serverTestProcess) where

import Control.Concurrent (threadDelay)
import System.IO.Silently
import System.Posix

import Hajong.Server

-- | Start a server process silenced
serverTestProcess :: IO ProcessID
serverTestProcess = do
    pid <- forkProcess $ hSilence [stdout, stderr] serverMain
    threadDelay 1000000
    return pid

serverTests :: TestTree
serverTests = testGroup "The websocket server"
    [
    ]
