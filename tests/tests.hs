------------------------------------------------------------------------------
-- | 
-- Module         : Main
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Main where

import System.Posix
import qualified Test.QuickCheck.Property as Q
import qualified Data.List as L

import GameMentsu
import GameMechanics
import CLIPrettyPrint
import Server
import CLIClient

import Hajong.Game
import Hajong.Server

main :: IO ()
main = do

    -- The server process used by the client tests
    putStrLn "Starting server process :9160..."
    pid <- serverTestProcess

    defaultMain tests `finally` do
        putStrLn "Killed server process."
        signalProcess killProcess pid

tests :: TestTree
tests = testGroup "Hajong tests"
    [ mentsuTests
    , mechanicsTests
    , cliPrettyPrintTests
    , clientTests
    ]
