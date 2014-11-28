{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
------------------------------------------------------------------------------
-- | 
-- Module         : HajongTest.Client.CLI
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module HajongTest.Client.CLI (tests) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.State  (StateT(..), evalStateT)
import Control.Monad.Reader (ReaderT(..), runReaderT)

import Hajong.Game
import Hajong.Client.CLI

-- | Tests for client side
tests :: TestTree
tests = testGroup "Client tests"
    [ clientCLILoungeTests
    , clientCLIGameTests
    ]

clientCLILoungeTests :: TestTree
clientCLILoungeTests = testGroup "CLI Lounge Unit Tests"
    [ testCase "connect + terminate on q"           $ runClient [   ]                           =~ "Connected to server"
    , testCase "? shows the help message"           $ runClient ["?"]                           =~ "Show this help text"
    , testCase "! shows status as lounge on start"  $ runClient ["!"]                           =~ "In lounge"
    , testCase "<Space>[message] gives prompt"      $ runClient [" ","The message"]             =~ "say:"
    , testCase "n lists idle users"                 $ runClient ["n"]                           =~ "Users idle:"
    , testCase "g lists games"                      $ runClient ["g"]                           =~ "ames"
    , testCase "c gives create prompt"              $ runClient ["c","Named"]                   =~ "Game name"
    , testCase "c doesn't accept empty name"        $ runClient ["c"," "]                       =~ "empty"
    , testCase "j opens join prompt"                $ runClient ["j","-1"]                      =~ "join game"  -- TODO
    , testCase "r sends a raw command"              $ runClient ["r","Message \"from\" \"me\""] =~ "send command"
    ]

clientCLIGameTests :: TestTree
clientCLIGameTests = testGroup "CLI In-Game unit tests"
    [ testCase "joining bogus game raises error"                      $ runClient ["j","-1" ] =~ "[error]"
    , testCase "joining game works (NOTE: requires game 0 on server)" $ runClient ["j","0"]   =~ "Joined the game"
    , testCase "game starts when 4 clients have joined" undefined
    ]

type ClientInputDummy = StateT (MVar Text, TChan Text) (ReaderT ClientState IO)

instance ClientOutput ClientInputDummy where
    outAll xs = do
        chan <- use _2
        liftIO . atomically $ mapM_ (writeTChan chan) xs

instance ClientInput ClientInputDummy where
    askChar   prompt   = out prompt >> use _1 >>= liftIO . takeMVar & liftM (Just . unsafeHead)
    withParam prompt f = out prompt >> use _1 >>= liftIO . takeMVar >>= f

runClient :: [Text] -> IO Text
runClient xs = do
    inputVar <- newEmptyMVar
    _ <- forkIO $ mapM_ (\x -> threadDelay 500000 >> putMVar inputVar x) $ xs ++ ["q"]
    runClient' inputVar <&> unlines

runClient' :: MVar Text -> IO [Text]
runClient' inputVar = do
    outputChan <- newBroadcastTChanIO
    outputReadChan <- atomically $ dupTChan outputChan

    let runDummy ma = runReaderT $ evalStateT ma (inputVar, outputChan)
        loop        = inputLoop loop

    clientMain' (runDummy loop) (runDummy clientReceiver)

    let readChan = tryReadTChan outputReadChan >>= maybe (return []) (\x -> (x :) <$> readChan)
        in atomically readChan
