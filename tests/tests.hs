{-# LANGUAGE NoImplicitPrelude #-}

import ClassyPrelude
import CLI
import System.IO.Silently
import Control.Concurrent
import Control.Concurrent.Async

testClient :: Handle -> IO String
testClient handle = do
    capture_ $ clientMain (Just handle)

main = do
    forkIO serverMain

    (c1, c2, c3, c4) <- runConcurrently $ (,,,)
        <$> Concurrently testClient
        <*> Concurrently testClient
        <*> Concurrently testClient
        <*> Concurrently testClient

    return ()
