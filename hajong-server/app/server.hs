------------------------------------------------------------------------------
-- | 
-- Module         : Main (server.hs)
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Main where

import Data.Text (pack)
import Hajong.Server
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import System.Log.FastLogger
import Network
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    -- TODO: We could read the hajong-site/production-settings.yaml here
    let (port, file, secret) = case args of
            []   -> (8001, "/tmp/hajong.socket", pack "secret")
            [x]  -> (8001, x, pack "secret")
            [x,y] -> (read x, y, pack "secret")
            [x,y,z] -> (read x, y, pack z)

    st  <- initServer port secret =<< newStderrLoggerSet defaultBufSize
    _   <- forkIO $ runServerMain st
    fin <- forkServerAcidRemote st (UnixSocket file)
    runServer st serverDebugger `finally` fin
