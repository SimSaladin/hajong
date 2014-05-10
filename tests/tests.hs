{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import ClassyPrelude hiding (assert)
import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent hiding (newMVar, withMVar, modifyMVar_)
import Data.Knob
import System.IO.Silently
import System.Timeout
import System.IO (hFlush, IOMode(..))
import System.Posix
import System.IO.Temp

import CLI

main = do
    putStrLn "Starting server process :9160..."
    pid <- startServer
    finally (defaultMain tests) $ do
        putStrLn "Killed server process."
        signalProcess killProcess pid

tests :: TestTree
tests =
    testGroup "Client tests" [clientCLILoungeTests, clientCLIGameTests]

clientCLILoungeTests :: TestTree
clientCLILoungeTests = testGroup "CLI Lounge Unit Tests"
    [ testCase "connect + terminate on q"           $ runClient "" =~ "Connected to server"
    , testCase "? shows the help message"           $ runClient "?" =~ "Show this help text"
    , testCase "! shows status as lounge on start"  $ runClient "!" =~ "In lounge"
    , testCase "<Space>[message] gives prompt"      $ runClient " The message\n" =~ "say:"
    , testCase "n lists idle users"                 $ runClient "n" =~ "Users idle:"
    , testCase "g lists games"                      $ runClient "g" =~ "Games"
    , testCase "c gives create prompt"              $ runClient "cNamed\n" =~ "Game name"
    , testCase "c doesn't accept empty name"        $ runClient "c \n" =~ "empty"
    , testCase "j opens join prompt"                $ runClient "j-1\n" =~ "join game"
    , testCase "r sends a raw command"              $ runClient "rMessage \"from\" \"me\"\n" =~ "send command"
    ]

clientCLIGameTests :: TestTree
clientCLIGameTests = testGroup "CLI In-Game unit tests"
    [ testCase "" undefined ]  -- $ gameClient "

(=~) :: IO Text -> Text -> IO ()
f =~ t = f >>= \res -> isInfixOf t res @? unpack (unlines ["== Got ==", res, "\n== Expected ==", t])

-- | Run a client with predefined input
runClient :: ByteString -> IO Text
runClient input = do
    (fpRes, hRes) <- openTempFile "/tmp" "hajong-test-res.log"
    hClose hRes

    pid <- forkProcess $ do
        k <- newKnob (input <> "q")
        (output, res) <- capture $ timeout 3000000 $ withFileHandle k "knob" ReadMode $ \h -> do
            clientMain $ Just h
            hFlush stdout
        writeFile (fpFromString fpRes) output

    _ <- getProcessStatus True False pid
    finally (readFile $ fpFromString fpRes) (removeLink fpRes)

-- | Start the server process silenced
startServer :: IO ProcessID
startServer = do
    pid <- forkProcess $ hSilence [stdout, stderr] serverMain
    threadDelay 1000000
    return pid
