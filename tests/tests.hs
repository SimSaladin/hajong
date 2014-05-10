{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import ClassyPrelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import Control.Concurrent
import Control.Concurrent.Async
import Data.Knob
import System.IO.Silently
import System.Timeout
import System.IO (hFlush, IOMode(..))
import System.Posix

import CLI

main = do
    putStrLn "Starting server process :9160..."
    pid <- startServer
    finally (defaultMain tests) $ do
        putStrLn "Killed server process."
        signalProcess killProcess pid

tests :: TestTree
tests =
    localOption (NumThreads 1) $
    testGroup "Client tests" [singleClientTests, fourClientTests]

singleClientTests :: TestTree
singleClientTests = testGroup "Single Client Unit Tests"
    [ testCase "client quits on q" $ do
        hFlush stdout
        res <- capture_ . testClient =<< newKnob "q"
        hFlush stdout
        "huh" @=? res
    ]

fourClientTests :: TestTree
fourClientTests = testGroup "Four clients playing"
    [ testCase "Initialize game" $ do
        hFlush stdout
        res <- fourClients
        hFlush stdout
        print res
        "--res--" @=? show res
        hFlush stdout
    ]

fourClients = do
    k0 <- newKnob "q"
    k1 <- newKnob "q"
    k2 <- newKnob "q"
    k3 <- newKnob "q"

    capture_ $ timeout 2000000 $ runConcurrently $ (,,,)
        <$> Concurrently (testClient k0)
        <*> Concurrently (testClient k1)
        <*> Concurrently (testClient k2)
        <*> Concurrently (testClient k2)

clientProcess :: IO ()
clientProcess = do
    k <- newKnob "q"
    pid <- forkProcess $ testClient k

-- | Start the server process silenced
startServer :: IO ProcessID
startServer = do
    pid <- forkProcess $ hSilence [stdout, stderr] serverMain
    threadDelay 1000000
    return pid

-- | read input from knob, resulting string 
testClient :: Knob -> IO ()
testClient knob = do
    hFlush stdout
    withFileHandle knob "client-input" ReadMode $ clientMain . Just
