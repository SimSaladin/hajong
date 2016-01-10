{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
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

import ClassyPrelude as X hiding (assert, Index, index, uncons, unsnoc, cons)
import Data.Maybe                         as X (fromJust)
import           Control.Lens             as X hiding (elements, snoc, (<.>))
import           Test.Tasty               as X
import           Test.Tasty.HUnit         as X
import           Test.Tasty.QuickCheck    as X
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Test.QuickCheck.Property as Q
import qualified Data.Set                 as Set
import qualified Data.Map                 as Map

import Mahjong hiding (elements)
import Mahjong.Hand.Internal (initHand)
import Hajong.Connections

-- | "action =~ expected" => expected `isInfixOf` (result of action)
(=~) :: IO Text -> Text -> IO ()
f =~ t = f >>= \res -> isInfixOf t res @? unpack (unlines ["== Got ==", res, "\n== Expected ==", t])

hunitAllInfixOf :: Text -> [Text] -> [TestTree]
hunitAllInfixOf res here =
   flip map here $ \x -> testCase ("Contains " <> unpack x) $ assertBool (unpack x <> " was not found") $ x `isInfixOf` res

propAllInfixOf :: Text -> [Text] -> Property
propAllInfixOf result xs = conjoin $ flip map xs $
    \x -> if x `isInfixOf` result
              then Q.succeeded
              else Q.failed { Q.reason = unpack $ x <> " not found in result" }

-- | xs .<-- ys succeeds when ys is contained within xs.
(.<--) :: (P.Pretty [a], Ord a) => [[a]] -> [[a]] -> Property
xs .<-- ys = conjoin $ isElem <$> ys
  where
      isElem y = counterexample
        (show $ P.pretty (sort y) P.<$$> " `notElem` " P.<$$> P.pretty xs')
        (sort y `elem` xs')
      xs'      = sort (map sort xs)

-- * Arbitrary instancees

instance Arbitrary (Hand Identity) where
    arbitrary = initHand <$> vector 13

instance Arbitrary Tile where
    arbitrary = elements riichiTiles

instance Arbitrary Kaze where arbitrary = arbitraryBoundedEnum

instance Arbitrary Number where arbitrary = arbitraryBoundedEnum

instance Arbitrary Mentsu where
    arbitrary = oneof
        [ kantsu <$> arbitrary
        , koutsu <$> arbitrary
        , jantou <$> arbitrary
        , arbitraryShuntsu
        ]

instance Arbitrary TileKind where
    arbitrary = elements [ManTile, PinTile, SouTile]

instance Arbitrary Player where
    arbitrary = Player <$> (arbitrary `suchThat` (>= 0))

instance Arbitrary Text where
    arbitrary = pack <$> arbitrary

instance Arbitrary GameAction where
    arbitrary = oneof
        [ GameTurn <$> arbitrary
        , GameShout <$> arbitrary
        , pure GameDontCare
        ]

instance Arbitrary TurnAction where
    arbitrary = oneof
        [ TurnTileDraw    <$> arbitrary <*> arbitrary
        , TurnAnkan       <$> arbitrary
        ]

instance Arbitrary Shout where
    arbitrary = Shout Ron <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary GameEvent where
    arbitrary = oneof
        [ DealTurnBegins <$> arbitrary
        , DealTurnAction <$> arbitrary <*> arbitrary
        , DealTurnShouted <$> arbitrary <*> arbitrary
        ]

instance Arbitrary Event where
    arbitrary = oneof
        [ {- JoinServer <$> arbitrary
        , PartServer <$> arbitrary
        , ClientIdentity <$> arbitrary
        , Message <$> arbitrary <*> arbitrary
        , Invalid <$> arbitrary

        , LoungeInfo <$> arbitrary
        , (\a b -> GameCreated (a,b,mempty)) <$> arbitrary <*> arbitrary
        , JoinGame <$> arbitrary <*> arbitrary
        , ForceStart <$> arbitrary

        , InGamePrivateEvent <$> arbitrary
        , InGameEvents <$> arbitrary -}
         InGameAction <$> arbitrary
        ]

instance Arbitrary TileGroup where
    arbitrary = oneof
        [ (\t -> GroupWait Koutsu [t,t] [t]) <$> arbitrary
        , breakShuntsu =<< arbitraryShuntsu
        , GroupComplete <$> arbitrary
        , GroupLeftover <$> arbitrary
        ] where
            breakShuntsu ms =
                let [t,t',t''] = mentsuTiles ms
                    in elements
                        [ GroupWait Shuntsu [t , t' ] (catMaybes [predMay t, Just t''])
                        , GroupWait Shuntsu [t', t''] (t : catMaybes [succMay t''])
                        , GroupWait Shuntsu [t , t''] [t']
                        ]

arbitraryShuntsu = do tile <- arbitrary `suchThat` (\tile -> isSuited tile && fromJust (tileNumber tile) <= Chii)
                      return $ shuntsu tile
