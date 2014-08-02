{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Prelude
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Prelude ( module Prelude, module X ) where

import ClassyPrelude as X hiding (assert, Index, uncons, unsnoc)
import Data.Maybe as X
import Control.Applicative as X
import Control.Lens as X hiding (elements, snoc, cons, (<.>))

import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.QuickCheck as X

import qualified Test.QuickCheck.Property as Q

import Hajong.Game

-- | "action =~ expected" => expected `isInfixOf` (result of action)
(=~) :: IO Text -> Text -> IO ()
f =~ t = f >>= \res -> isInfixOf t res @? unpack (unlines ["== Got ==", res, "\n== Expected ==", t])

hunitAllInfixOf :: Text -> [Text] -> [TestTree]
hunitAllInfixOf res exp =
   flip map exp $ \x -> testCase ("Contains " <> unpack x) $ assertBool (unpack x <> " was not found") $ x `isInfixOf` res

propAllInfixOf :: Text -> [Text] -> Property
propAllInfixOf result xs = conjoin $ flip map xs $
    \x -> if x `isInfixOf` result
              then Q.succeeded
              else Q.failed { Q.reason = unpack $ x <> " not found in result" }

-- * Arbitrary instancees

instance Arbitrary Hand where
    arbitrary = initHand <$> vector 13

instance Arbitrary Tile where
    arbitrary = elements riichiTiles

instance Arbitrary Mentsu where
    arbitrary = oneof
        [ arbitrary >>= \tile -> return (Kantsu (replicate 4 tile) Nothing)
        , arbitrary >>= \tile -> return (Koutsu (replicate 3 tile) Nothing)
        , arbitrary >>= \tile -> return (Jantou (replicate 2 tile) Nothing)
        , do
             tile <- arbitrary `suchThat` (\tile -> tileSuited tile && tileNumber tile <= Chii)
             let n = tileNumber tile
             return $ Shuntsu (tile : map (setTileNumber tile) [succ n, succ (succ n)]) Nothing
        ]

instance Arbitrary Player where
    arbitrary = elements defaultPlayers
