{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import ClassyPrelude hiding (assert)
import Control.Concurrent hiding (newMVar, withMVar, modifyMVar_)
import Control.Concurrent.Async
import Data.Knob
import System.IO (hFlush, IOMode(..))
import System.IO.Silently
import System.IO.Temp
import System.Posix
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import CLI

main :: IO ()
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
    , testCase "j opens join prompt"                $ runClient "j-1\n" =~ "join game"  -- TODO
    , testCase "r sends a raw command"              $ runClient "rMessage \"from\" \"me\"\n" =~ "send command"
    ]

clientCLIGameTests :: TestTree
clientCLIGameTests = testGroup "CLI In-Game unit tests"
    [ testCase "joining bogus game raises error"                      $ runClient "j-1\n" =~ "[error]"
    , testCase "joining game works (NOTE: requires game 0 on server)" $ runClient "j0\n"  =~ "Ready to start game 0"
    , testCase "game starts when 4 clients have joined" $ return ()
        -- FIXME: It is too much asked that that this could actually work like
        -- this. forkProcess is too tricky; cannot async them: the
        -- following actually runs every runClient sequentically. Or, if
        -- there's a print in runClient the whole thing just freezes.
        --
        -- Solution: Separete input and handling in CLI, or run the clients
        -- via System.Process.
        -- 
        -- (a, b, c, d) <- runConcurrently $ (,,,)
        --     <$> Concurrently (runClient "j0\n__")
        --     <*> Concurrently (runClient "j0\n__")
        --     <*> Concurrently (runClient "j0\n__")
        --     <*> Concurrently (runClient "j0\n__")
        -- putStrLn a
        -- putStrLn b
        -- putStrLn c
        -- putStrLn d
    ]

(=~) :: IO Text -> Text -> IO ()
f =~ t = f >>= \res -> isInfixOf t res @? unpack (unlines ["== Got ==", res, "\n== Expected ==", t])

-- | Run a client with predefined input
runClient :: ByteString -> IO Text
runClient input = do
    putStrLn "runClient"
    (fpRes, hRes) <- openTempFile "/tmp" "hajong-test-res.log"
    hClose (fpRes `seq` hRes)

    pid <- forkProcess $ clientProcess fpRes input
    _ <- getProcessStatus True False pid

    finally (readFile $ fpFromString fpRes) (removeLink fpRes)

clientProcess :: String -> ByteString -> IO ()
clientProcess fp input = do
    k <- newKnob (input <> "q")
    (output, res) <-
        withFileHandle k "knob" ReadMode $
        capture . timeout 3000000 . clientMain . Just
    writeFile (fpFromString fp) (output <> if isNothing res then "Client didn't terminate" else "")


-- | Start the server process silenced
startServer :: IO ProcessID
startServer = do
    pid <- forkProcess $ hSilence [stdout, stderr] serverMain
    threadDelay 1000000
    return pid
