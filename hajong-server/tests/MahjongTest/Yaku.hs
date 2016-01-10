------------------------------------------------------------------------------
-- | 
-- Module         : MahjongTest.Yaku
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module MahjongTest.Yaku where

import Prelude hiding (Discard)
import Mahjong.Hand.Internal  (initHand)
import Mahjong
import MahjongTest.Mechanics

-- | Defaults for the tests
kyoku = Kyoku {_pRound = (Ton, 1, 0), _pTurn = Ton, _pOja = Player 3, _pFirstOja = Player 3, _pWallTilesLeft = 70, _pDora = [], _pFlags = setFromList [OpenedUraDora []], _pPlayers = error "not used", _pHonba = 0, _pRiichi = 0, _pResults = Nothing, _sEventHistory = [], _sHands = error "not used", _sWall = [], _sWanpai = Wanpai [] [] ["M1"] [], _sWaiting = Nothing}
valueInfo = ValueInfo kyoku Ton $ initHand []

tests :: TestTree
tests = testGroup "Standard Yaku"
    [ testCase "Pinfu" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M1" ["M2", "M3"])
        snd (getYaku vi) @?= [Yaku 1 "Pinfu"]

    , testCase "Pinfu NOT on end wait " $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
        snd (getYaku vi) @?= []

    , testCase "Iipeikou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "M3", "S7", "S8", "S9", "S1", "S2", "S3", "M5", "M5"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
        snd (getYaku vi) @?= [Yaku 1 "Iipeikou"]

    , testCase "Ryanpeikou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "M3", "S7", "S8", "S9", "S7", "S8", "S9", "M5", "M5"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
        snd (getYaku vi) @?= [Yaku 3 "Ryanpeikou"]

    , testCase "Sanshoku Doujin" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P1", "P2", "P3", "S7", "S8", "S9", "S1", "S2", "S3", "M5", "M5"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
        snd (getYaku vi) @?= [Yaku 2 "Sanshoku Doujin"]

    , testCase "Ittsuu" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M4", "M5", "M6", "M7", "M8", "M9", "S7", "S8", "S9", "W", "W"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M1" ["M2", "M3"])
        snd (getYaku vi) @?= [Yaku 2 "Ittsuu"]

    , testCase "Honroutou (+ Toitoi)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S1", "S1", "S1", "S9", "S9", "W", "W"]
                           & vHand.handCalled .~ [fromShout $ Shout Pon Nan "M1" ["M1", "M1"], fromShout $ Shout Pon Nan "M9" ["M9", "M9"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "S9" ["S9", "S9"])
        snd (getYaku vi) @?= [Yaku 2 "Honroutou", Yaku 2 "Toitoi"]

    , testCase "Toitoi" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S2", "S2", "S2", "S9", "S9", "W", "W"]
                           & vHand.handCalled .~ [fromShout $ Shout Pon Nan "M1" ["M1", "M1"], fromShout $ Shout Pon Nan "M9" ["M9", "M9"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "S9" ["S9", "S9"])
        snd (getYaku vi) @?= [Yaku 2 "Toitoi"]

    , testCase "San ankou (+ Toitoi)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "M1", "S1", "S1", "S1", "S2", "S2", "S2", "W", "W"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "S9" ["S9", "S9"])
        snd (getYaku vi) @?= [Yaku 2 "San ankou", Yaku 2 "Toitoi"]

    , testCase "San ankou NOT when discard is part of the third set" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "M1", "S1", "S1", "S1", "W", "W"]
                           & vHand.handCalled .~ [fromShout $ Shout Chi Nan "S7" ["S8", "S9"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "S2" ["S2", "S2"])
        snd (getYaku vi) @?= []

    , testCase "San kantsu (+ Toitoi)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["W", "W"]
                           & vHand.handCalled .~ [ Mentsu Kantsu ["M1","M1","M1","M1"] $ Just $ Shout Pon Nan "M1" ["M1", "M1"]
                                                 , Mentsu Kantsu (replicate 4 "M9") $    Just $ Shout Pon Nan "M9" ["M9", "M9"]
                                                 , Mentsu Kantsu (replicate 4 "S2") $    Just $ Shout Pon Nan "S2" ["S2", "S2"] ]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "S9" ["S7", "S8"])
        snd (getYaku vi) @?= [Yaku 2 "San kantsu"]

    , testCase "San Shoku Doukou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S1", "S1", "S1", "P1", "P1", "P1", "W", "W"]
                           & vHand.handCalled .~ [fromShout $ Shout Pon Nan "M1" ["M1", "M1"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "S3" ["S5", "S4"])
        snd (getYaku vi) @?= [Yaku 2 "San Shoku Doukou"]

    , testCase "Shou Sangen + 2 x Yakuhai" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["R!", "R!"]
                           & vHand.handCalled .~ [fromShout $ Shout Pon Nan "G!" ["G!", "G!"], fromShout $ Shout Pon Nan "W!" ["W!", "W!"], fromShout $ Shout Pon Nan "P5" ["P5", "P5"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
        snd (getYaku vi) @?= [Yaku 1 "Green", Yaku 1 "White", Yaku 2 "Shou Sangen"]

    , testCase "Tanyao" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P2", "P3", "P4", "S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M2" ["M2", "M2"])
        snd (getYaku vi) @?= [Yaku 1 "Tanyao"]

    , testCase "Chanta" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P1", "P2", "P3", "S7", "S8", "S9", "M7", "M8", "M9", "W", "W"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
        snd (getYaku vi) @?= [Yaku 2 "Chanta"]

    , testCase "Chanta (open)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S7", "S8", "S9", "M7", "M8", "M9", "W", "W"]
                           & vHand.handCalled .~ [fromShout $ Shout Chi Nan "P1" ["P2", "P3"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M1" ["M2", "M3"])
        snd (getYaku vi) @?= [Yaku 1 "Chanta"]

    , testCase "Kuitan" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handCalled .~ [fromShout $ Shout Chi Nan "P2" ["P3", "P4"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M2" ["M2", "M2"])
        snd (getYaku vi) @?= [Yaku 1 "Kuitan"]

    , testCase "Honitsu (open)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M5", "M6", "M7", "M2", "M3", "M4", "W", "W"]
                           & vHand.handCalled .~ [fromShout $ Shout Chi Nan "M4" ["M5" , "M6"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M2" ["M2", "M2"])
        snd (getYaku vi) @?= [Yaku 2 "Honitsu"]

    , testCase "Junchan" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P1", "P2", "P3", "S7", "S8", "S9", "M7", "M8", "M9", "M9", "M9"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
        snd (getYaku vi) @?= [Yaku 3 "Junchan"]

    , testCase "Chinitsu (open)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M5", "M6", "M7", "M2", "M3", "M4", "M9", "M9"]
                           & vHand.handCalled .~ [fromShout $ Shout Chi Nan "M4" ["M5", "M6"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M2" ["M2", "M2"])
        snd (getYaku vi) @?= [Yaku 5 "Chinitsu"]

    , testCase "Chiitoitsu" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M2", "M5", "M5", "M7", "M7", "P3", "P3", "M9", "M9", "M1", "M1"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M3"])
        snd (getYaku vi) @?= [Yaku 2 "Chiitoitsu"]

    , testCase "Menzen Tsumo" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S2", "S3", "S4", "S9", "S9"]
                           & vHand.handAgari .~ Just (AgariTsumo "M2" False)
        snd (getYaku vi) @?= [Yaku 1 "Menzen Tsumo"]

    , testCase "Riichi" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
                           & vHand.handRiichi .~ Riichi
        snd (getYaku vi) @?= [Yaku 1 "Riichi"]

    , testCase "Double Riichi" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
                           & vHand.handRiichi .~ DoubleRiichi
        snd (getYaku vi) @?= [Yaku 2 "Double riichi"]

    , testCase "Ippatsu" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M1", "M2"])
                           & vHand.handRiichi .~ Riichi
                           & vHand.handIppatsu .~ True
        sort (snd (getYaku vi)) @?= sort [Yaku 1 "Riichi", Yaku 1 "Ippatsu"]

    , testCase "Houtei Raoyui" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handCalled .~ [fromShout $ Shout Chi Nan "P2" ["P3", "P4"]]
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M1" ["M1", "M1"])
                           & vKyoku.pWallTilesLeft .~ 0
        snd (getYaku vi) @?= [Yaku 1 "Houtei Raoyui"]

    , testCase "Haitei Raoyui" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handCalled .~ [fromShout $ Shout Chi Nan "P2" ["P3", "P4"]]
                           & vHand.handAgari .~ Just (AgariTsumo "M1" False)
                           & vKyoku.pWallTilesLeft .~ 0
        snd (getYaku vi) @?= [Yaku 1 "Haitei Raoyui"]

    , testCase "Rinshan Kaihou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handCalled .~ [fromShout $ Shout Chi Nan "P2" ["P3", "P4"]]
                           & vHand.handAgari .~ Just (AgariTsumo "M1" True)
        snd (getYaku vi) @?= [Yaku 1 "Rinshan Kaihou"]

    , testCase "Dora, ura-dora and aka-dora are counted when calculating hand" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P2", "P3", "P4", setAka "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handCalled .~ []
                           & vHand.handAgari .~ Just (AgariCall $ Shout Ron Nan "M3" ["M4", setAka "M5"])
                           & vHand.handRiichi .~ Riichi
                           & vKyoku.pDora .~ ["S1"]
                           & vKyoku.pFlags .~ setFromList [OpenedUraDora [TileEq "P1"]]

        snd (getYaku vi) @?= [Yaku 1 "Riichi", Yaku 1 "Pinfu", YakuExtra 1 "Dora", YakuExtra 1 "Ura-Dora", YakuExtra 2 "Aka-Dora"]

    -- Yakumans

    , testCase "Daisangen" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ replicate 2 "G" ++ replicate 3 "W!" ++ replicate 3 "R" ++ ["M2", "M3", "M4", "M1", "M1"]
                           & vHand.handAgari .~ Just (AgariTsumo "G" False)
        snd (getYaku vi) @?= [Yaku 13 "Daisangen"]

    , testCase "Kokushi Musou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P1", "P9", "M1", "M9", "S1", "S9", "E", "S", "W", "N", "G", "R", "W!"]
                           & vHand.handAgari .~ Just (AgariTsumo "G" False)
        snd (getYaku vi) @?= [Yaku 13 "Kokushi Musou"]

    , testCase "Suuankou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ replicate 2 "N" ++ replicate 3 "W!" ++ replicate 3 "R" ++ ["M2", "M2", "M2", "M1", "M1"]
                           & vHand.handAgari .~ Just (AgariTsumo "N" False)
        snd (getYaku vi) @?= [Yaku 13 "Suuankou"]

    , testCase "Shousuushii" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ replicate 2 "E" ++ replicate 3 "S" ++ replicate 3 "W" ++ replicate 2 "N" ++ ["M1", "M2", "M3"]
                           & vHand.handAgari .~ Just (AgariTsumo "E" False)
        snd (getYaku vi) @?= [Yaku 13 "Shousuushii"]

    , testCase "Daisuushii" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ replicate 3 "E" ++ replicate 3 "S" ++ replicate 3 "W" ++ replicate 2 "N" ++ ["M1", "M2", "M3"]
                           & vHand.handAgari .~ Just (AgariTsumo "N" False)
        snd (getYaku vi) @?= [Yaku 13 "Suuankou", Yaku 13 "Daisuushii"]

    , testCase "Tsuuiisou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ replicate 1 "E" ++ replicate 3 "S" ++ replicate 3 "W" ++ replicate 3 "G" ++ replicate 3 "R"
                           & vHand.handAgari .~ Just (AgariTsumo "E" False)
        snd (getYaku vi) @?= [Yaku 13 "Suuankou", Yaku 13 "Tsuuiisou"]

    , testCase "Chinroutou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ replicate 3 "M1" ++ replicate 3 "M9" ++ replicate 3 "S1" ++ replicate 3 "S9" ++ ["P1"]
                           & vHand.handAgari .~ Just (AgariTsumo "P1" False)
        snd (getYaku vi) @?= [Yaku 13 "Suuankou", Yaku 13 "Chinroutou"]

    , testCase "Chuuren Poutou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P1","P1","P1","P2","P3","P4","P5","P6","P7","P8","P9","P9","P9"]
                           & vHand.handAgari .~ Just (AgariTsumo "P4" False)
        snd (getYaku vi) @?= [Yaku 13 "Chuuren Poutou"]

    , testCase "Kazoe Yakuman" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["P1", "P1", "P1", "P2", "P3", "P5", "P5", "P5", "P7", "P7", "P7", "P9", "P9"]
                           & vHand.handAgari .~ Just (AgariTsumo "P1" False)
                           & vKyoku.pDora .~ ["P9", "P2"]
        snd (getYaku vi) @?= [Yaku 13 "Kazoe Yakuman"]

    -- NOTE: implemented in Mechanics test, for now
    -- , testCase "Nagashi Mangan" $ do
    -- , testCase "Chankan" $ do error "test not implemented (here) (yet)"
    -- , testCase "Suu Kantsu" _

    ]
