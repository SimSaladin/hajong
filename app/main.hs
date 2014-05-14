module Main where

import ClassyPrelude
import Server
import CLI

main = hajongCLI

hajongCLI :: IO ()
hajongCLI = do
    args <- getArgs
    case args of 
        (x:_) | "s" `isPrefixOf` x -> server
              | "c" `isPrefixOf` x -> client
        _ -> do
            putStrLn "Hajong v0. Type s for server, c for client."
            inp <- T.getLine
            case inp of
                "s" -> server
                _   -> client
    where
        server = putStrLn "Starting server..." >> serverMain
        client = putStrLn "Starting client..." >> clientMain Nothing
