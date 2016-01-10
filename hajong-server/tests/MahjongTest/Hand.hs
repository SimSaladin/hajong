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

import Mahjong.Tiles
import Mahjong.Hand

tests :: TestTree
tests = testGroup "General hand operations"
    [ testGroup "shoutsOn (possible shouts)"
        [ testCase "Ron on jantou wait" $ do
            let h = Hand {_handCalled = []
                         , _handDiscards = [Mahjong.Hand.Discard {_dcTile = "P5", _dcTo = Nothing, _dcRiichi = False}]
                         , _handRiichi = NoRiichi, _handIppatsu = False, _handState = DrawNone, _handPicks = []
                         , _handConcealed = ["M1", "M2", "M3", "P1", "P2", "P3", "P5", "P7", "P8", "P9", "S2", "S3", "S4"]
                         , _handFuriten = NotFuriten, _handCanTsumo = False }
                res = shoutsOn Ton "P5" Nan h
            res == [Shout Ron Ton "P5" ["P5"]] @? "Expected: Ron Received: " <> show res
        ]
    ]
