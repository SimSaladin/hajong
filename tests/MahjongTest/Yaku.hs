------------------------------------------------------------------------------
-- | 
-- Module         : MahjongTest.Yaku
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module MahjongTest.Yaku where

import Mahjong.Hand.Yaku

tests :: TestTree
tests = testGroup "Yaku"
    [ testCase "Pinfu" $
        toInfo "M1" ["M2", "M3", "P2", "P3", "P4", "S5", "S6", "S7", "S1",
                     "S2", "S4" , "S9", "S9"] [] `shouldBe`
                     ["Pinfu"]
    ]

-- |
-- @
-- toInfo tileWonWith tilesInHand openMentsu
-- @
toInfo :: Tile -> [Tile] -> [Mentsu] -> ValueInfo
toInfo winwith tiles mentsu =
    ValueInfo Ton Ton False True tiles mentsu winwith

-- |
-- @
-- toInfo agari inhand open `shouldBe` yaku
-- @
--
-- (See exact yaku names from src/Hand/Yaku/Standard.hs)
shouldBe :: ValueInfo -> [Text] -> TestCase
shouldBe toTest compTo =
    sort (map yakuName (getYaku withThis)) @?= sort compTo
