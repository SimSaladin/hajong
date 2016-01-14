------------------------------------------------------------------------------
-- |
-- Module         : Main
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Main (main, tests) where

import System.Posix
import qualified Test.QuickCheck.Property as Q

import qualified MahjongTest.Mentsu          as Mentsu
import qualified MahjongTest.Hand            as Hand
import qualified MahjongTest.Mechanics       as Mechanics
import qualified MahjongTest.Yaku            as Yaku

-- import qualified HajongTest.CLI.PrettyPrint  as PrettyPrint
-- import qualified HajongTest.CLI.Client       as Client
import qualified HajongTest.Server.Server    as Server
import qualified HajongTest.Server.Worker    as Worker

main :: IO ()
main = do
    -- The server process used by the client tests
    putStrLn "Starting server process :9160..."
    _st <- Server.forkTestServer

    defaultMain tests

tests :: TestTree
tests = testGroup "Hajong tests"
    [ Mentsu.tests
    , Yaku.tests
    , Mechanics.tests
    , Hand.tests
    --, PrettyPrint.tests
    --, Client.tests
    , Worker.tests
    ]
