------------------------------------------------------------------------------
-- |
-- Module         : HajongTest.Server.Server
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module HajongTest.Server.Server (tests, forkTestServer) where

import Control.Concurrent (threadDelay)
import System.IO.Silently
import System.Posix
import System.Log.FastLogger

import Hajong.Server
import Hajong.Server.Config
import Hajong.Server.Internal (ServerSt)

tests :: TestTree
tests = testGroup "The websocket server"
    [
    ]

serverTestConf = ServerConfig
    { _websocketHost       = "127.0.0.1"
    , _websocketPort       = 9160
    , _databaseSocket      = "/tmp/hajong-test.socket"
    , _logDirectory        = "/tmp/hajong-test"
    , _websocketCtrlSecret = "test-secret"
    }

forkTestServer :: IO ServerSt
forkTestServer = serverStartParts serverTestConf
