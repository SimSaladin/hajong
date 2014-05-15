{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Mytest
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mytest where

import ClassyPrelude hiding (assert)
import Control.Lens
import Control.Concurrent hiding (newMVar, withMVar, modifyMVar_)
import Data.Knob
import System.IO (IOMode(..))
import System.IO.Silently
import System.IO.Temp
import System.Posix
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import CLI
import Server
import Riichi


main = putStrLn "Mui."
