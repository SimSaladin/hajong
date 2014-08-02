------------------------------------------------------------------------------
-- | 
-- Module         : GameMentsu
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GameMentsu (mentsuTests) where

import Data.Maybe

import Hajong.Game
import Hajong.Client.PrettyPrint

mentsuTests :: TestTree
mentsuTests = testGroup "Mentsu tests"
    [ testCase "M1-M1-M1 is koutsu" $ mentsuAssert "M1 M1 M1" [[koutsu $ pread "M1 M1 M1"]]

    , testProperty "Any triplet is koutsu" $ \t ->
        let tiles = replicate 3 t
            in mentsuAre tiles [[koutsu tiles]]

    , testProperty "Any quadret is koutsu and kantsu" $ \t ->
        let tiles = replicate 4 t
            in mentsuAre tiles [[kantsu tiles], replicate 2 (jantou $ take 2 tiles)]

    , testProperty "Any a, a+1, a+2 sequence (for a <= 7) of tiles is a single shuntsu" $ \tile ->
        let tiles = [tile, fromJust (tileSucc tile), fromJust (tileSucc tile >>= tileSucc)]
            in tileSuited tile && tileNumber tile <= Chii
                ==> mentsuAre tiles [[shuntsu tiles]]

    , testProperty "Two koutsu, different suit" $ \(t, r) ->
        let tiles = replicate 3 t ; riles = replicate 3 r
            in not (compareSuit t r)
                ==> mentsuAre (tiles ++ riles) [[koutsu tiles, koutsu riles]]
    ]

mentsuAre :: [Tile] -> [[Mentsu]] -> Property
mentsuAre tiles expected = sort (map sort calculated) === sort (map sort expected)
    where calculated = getMentsu tiles

mentsuAssert :: Text -> [[Mentsu]] -> Assertion
mentsuAssert txt xs =
        let tiles = pread txt
            in getMentsu tiles @?= xs
