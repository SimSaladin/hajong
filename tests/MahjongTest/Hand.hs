------------------------------------------------------------------------------
-- | 
-- Module         : MahjongTest.Hand
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module MahjongTest.Hand where

import Mahjong.Hand

tests :: TestTree
tests = testGroup "General hand operations"
    [ testGroup "shoutsOn (possible shouts)"
        [ testProperty "M1 -> ..,M1,..,M1,.." $ \(f, ts) -> 
            shoutsOn f "M1" (ts ++ ["M1", "M1"]) .<-- [["M1", "M1"]]
        , testProperty "M1 -> M2, M3, True" $ \ts ->
            shoutsOn True "M1" (ts ++ ["M2", "M3"]) .<-- [["M2", "M3"]]
        ]
    ]
