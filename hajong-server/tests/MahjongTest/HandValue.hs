------------------------------------------------------------------------------
-- | 
-- Module         : MahjongTest.HandValue
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module MahjongTest.HandValue where

import Mahjong.Hand.Mentsu
import Mahjong.Hand.Value

tests :: TestTree
tests = testGroup "Hand value"
    [ testGroup "`mentsuValue` (Fu)"
        [ testCase " M5 minkoutsu          " $ minkoutsu "M5" @?= 2
        , testCase " M5 ankoutsu           " $ ankoutsu  "M5" @?= 4
        , testCase " M1 terminal minkoutsu " $ minkoutsu "M1" @?= 4
        , testCase " M1 terminal ankoutsu  " $ ankoutsu  "M1" @?= 8
        , testCase " G honor minkoutsu     " $ minkoutsu "G " @?= 4
        , testCase " G honor ankoutsu      " $ ankoutsu  "G " @?= 8

        , testCase " M5 minkantsu          " $ minkantsu "M5" @?= 8
        , testCase " M5 ankantsu           " $ ankantsu  "M5" @?= 16
        , testCase " M1 terminal minkantsu " $ minkantsu "M1" @?= 16
        , testCase " M1 terminal ankantsu  " $ ankantsu  "M1" @?= 32
        , testCase " G honor minkantsu     " $ minkantsu "G " @?= 16
        , testCase " G honor ankantsu      " $ ankantsu  "G " @?= 32

        , testCase " M1 jantou "            $ mentsuValue (jantou "M1") @?= 0 
        , testCase " G jantou "             $ mentsuValue (jantou "G") @?= 0 

        , testProperty "Fu of any shuntsu is 0" $ (== 0) . mentsuValue <$> arbitraryShuntsu
        ]
    ]

ankoutsu  = mentsuValue . koutsu
minkoutsu = mentsuValue . fromShout . Pon undefined

ankantsu  = mentsuValue . kantsu
minkantsu = mentsuValue . fromShout . Kan undefined

