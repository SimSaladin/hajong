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

import Hajong.Server
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import System.Log.FastLogger
import Network
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let (port, file) = case args of
            []   -> (8001, "/tmp/hajong.socket")
            [x]  -> (8001, x)
            [x,y] -> (read x, y)

    st  <- initServer port =<< newStderrLoggerSet defaultBufSize
    _   <- forkIO $ runServerMain st
    fin <- forkServerAcidRemote st (UnixSocket file)
    runServer st serverDebugger `finally` fin
