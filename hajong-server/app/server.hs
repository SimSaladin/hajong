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
import System.Log.FastLogger

main :: IO ()
main = do
    st <- initServer =<< newStderrLoggerSet defaultBufSize
    _ <- forkIO $ runServerMain st
    runServer st serverDebugger
