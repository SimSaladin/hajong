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
import Mahjong
import MahjongTest.Mechanics

-- | Defaults for the tests
kyoku = Kyoku {_pRound = (Ton, 1), _pTurn = Ton, _pOja = Player 3, _pFirstOja = Player 3, _pWallTilesLeft = 70, _pDora = [], _pFlags = setFromList [OpenedUraDora []], _pPlayers = error "not used", _pHonba = 0, _pRiichi = 0, _pResults = Nothing, _pDeals = [], _sEvents = [], _sHands = error "not used", _sWall = [], _sWanpai = Wanpai [] [] ["M1"] [], _sWaiting = Nothing}
valueInfo = ValueInfo kyoku Ton $ Hand [] (map (\t -> Discard t Nothing False) ["N" ,"W" ,"G!","S7","P9","M6","S2","R!","S9","P1"]) NoRiichi False DrawNone [AgariTsumo "P2"] (return ["S5","P6","S6","P5","P4","S4","M5","S2","P3","P7","S7","S3","M5"]) (return NotFuriten) (return False) (pure mempty)

tests :: TestTree
tests = testGroup "Standard Yaku"
    [ testCase "Pinfu" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M3", "P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handPicks .~ [AgariCall "M1" Nan]
        snd (getYaku vi) @?= [Yaku 1 "Pinfu"]

    , testCase "Pinfu NOT on end wait " $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
        snd (getYaku vi) @?= []

    , testCase "Iipeikou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "M1", "M2", "M3", "S7", "S8", "S9", "S1", "S2", "S3", "M5", "M5"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
        snd (getYaku vi) @?= [Yaku 1 "Iipeikou"]

    , testCase "Ryanpeikou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "M1", "M2", "M3", "S7", "S8", "S9", "S7", "S8", "S9", "M5", "M5"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
        snd (getYaku vi) @?= [Yaku 3 "Ryanpeikou"]

    , testCase "Sanshoku Doujin" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P1", "P2", "P3", "S7", "S8", "S9", "S1", "S2", "S3", "M5", "M5"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
        snd (getYaku vi) @?= [Yaku 2 "Sanshoku Doujin"]

    , testCase "Ittsuu" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "S7", "S8", "S9", "W", "W"]
                           & vHand.handPicks .~ [AgariCall "M1" Nan]
        snd (getYaku vi) @?= [Yaku 2 "Ittsuu"]

    , testCase "Honroutou (+ Toitoi)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S1", "S1", "S1", "S9", "S9", "W", "W"]
                           & vHand.handCalled .~ [Mentsu Koutsu "M1" (Just (Shout{shoutKind = Pon})), Mentsu Koutsu "M9" (Just (Shout{shoutKind = Pon}))]
                           & vHand.handPicks .~ [AgariCall "S9" Nan]
        snd (getYaku vi) @?= [Yaku 2 "Honroutou", Yaku 2 "Toitoi"]

    , testCase "Toitoi" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S2", "S2", "S2", "S9", "S9", "W", "W"]
                           & vHand.handCalled .~ [Mentsu Koutsu "M1" (Just (Shout{shoutKind = Pon})), Mentsu Koutsu "M9" (Just (Shout{shoutKind = Pon}))]
                           & vHand.handPicks .~ [AgariCall "S9" Nan]
        snd (getYaku vi) @?= [Yaku 2 "Toitoi"]

    , testCase "San ankou (+ Toitoi)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "M1", "S1", "S1", "S1", "S2", "S2", "S2", "S9", "S9", "W", "W"]
                           & vHand.handPicks .~ [AgariCall "S9" Nan]
        snd (getYaku vi) @?= [Yaku 2 "San ankou", Yaku 2 "Toitoi"]

    , testCase "San ankou NOT when discard is part of the third set" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "M1", "S1", "S1", "S1", "W", "W"]
                           & vHand.handCalled .~ [Mentsu Shuntsu "S7" (Just $ Shout Chi Nan "S7" ["S8", "S9"]), Mentsu Koutsu "S2" (Just $ Shout Ron Nan "S2" ["S2", "S2"])]
                           -- & vHand.handPicks .~ [AgariCall "S2" Nan]
        snd (getYaku vi) @?= []

    , testCase "San kantsu (+ Toitoi)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S7", "S8", "W", "W"]
                           & vHand.handPicks .~ [AgariCall "S9" Nan]
                           & vHand.handCalled .~ [Mentsu Kantsu "M1" (Just (Shout{shoutKind = Pon})), Mentsu Kantsu "M9" (Just (Shout{shoutKind = Pon})), Mentsu Kantsu "S2" (Just (Shout{shoutKind = Pon}))]
        snd (getYaku vi) @?= [Yaku 2 "San kantsu"]

    , testCase "San Shoku Doukou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["S1", "S1", "S1", "P1", "P1", "P1", "S5", "S4", "W", "W"]
                           & vHand.handPicks .~ [AgariCall "S3" Nan]
                           & vHand.handCalled .~ [Mentsu Koutsu "M1" (Just (Shout{shoutKind = Pon}))]
        snd (getYaku vi) @?= [Yaku 2 "San Shoku Doukou"]

    , testCase "Shou Sangen + 2 x Yakuhai" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["R!", "R!", "M1", "M2"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
                           & vHand.handCalled .~ [Mentsu Koutsu "G!" (Just (Shout{shoutKind = Pon})), Mentsu Koutsu "W!" (Just (Shout{shoutKind = Pon})), Mentsu Koutsu "P5" (Just (Shout{shoutKind = Pon}))]
        snd (getYaku vi) @?= [Yaku 1 "Green", Yaku 1 "White", Yaku 2 "Shou Sangen"]

    , testCase "Tanyao" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handPicks .~ [AgariCall "M2" Nan]
        snd (getYaku vi) @?= [Yaku 1 "Tanyao"]

    , testCase "Chanta" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P1", "P2", "P3", "S7", "S8", "S9", "M7", "M8", "M9", "W", "W"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
        snd (getYaku vi) @?= [Yaku 2 "Chanta"]

    , testCase "Chanta (open)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M3", "S7", "S8", "S9", "M7", "M8", "M9", "W", "W"]
                           & vHand.handCalled .~ [Mentsu Shuntsu "P1" (Just (Shout{shoutKind = Chi}))]
                           & vHand.handPicks .~ [AgariCall "M1" Nan]
        snd (getYaku vi) @?= [Yaku 1 "Chanta"]

    , testCase "Kuitan" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M2", "S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handCalled .~ [Mentsu Shuntsu "P2" (Just (Shout{shoutKind = Chi}))]
                           & vHand.handPicks .~ [AgariCall "M2" Nan]
        snd (getYaku vi) @?= [Yaku 1 "Kuitan"]

    , testCase "Honitsu (open)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M2", "M5", "M6", "M7", "M2", "M3", "M4", "W", "W"]
                           & vHand.handCalled .~ [Mentsu Shuntsu "M4" (Just (Shout{shoutKind = Chi}))]
                           & vHand.handPicks .~ [AgariCall "M2" Nan]
        snd (getYaku vi) @?= [Yaku 2 "Honitsu"]

    , testCase "Junchan" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P1", "P2", "P3", "S7", "S8", "S9", "M7", "M8", "M9", "M9", "M9"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
        snd (getYaku vi) @?= [Yaku 3 "Junchan"]

    , testCase "Chinitsu (open)" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M2", "M5", "M6", "M7", "M2", "M3", "M4", "M9", "M9"]
                           & vHand.handCalled .~ [Mentsu Shuntsu "M4" (Just (Shout{shoutKind = Chi}))]
                           & vHand.handPicks .~ [AgariCall "M2" Nan]
        snd (getYaku vi) @?= [Yaku 5 "Chinitsu"]

    , testCase "Chiitoitsu" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M2", "M5", "M5", "M7", "M7", "P3", "P3", "M9", "M9", "M1", "M1", "M3"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
        snd (getYaku vi) @?= [Yaku 2 "Chiitoitsu"]

    , testCase "Menzen Tsumo" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M2", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S2", "S3", "S4", "S9", "S9"]
                           & vHand.handPicks .~ [AgariTsumo "M2"]
        snd (getYaku vi) @?= [Yaku 1 "Menzen Tsumo"]

    , testCase "Riichi" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
                           & vHand.handRiichi .~ Riichi
        snd (getYaku vi) @?= [Yaku 1 "Riichi"]

    , testCase "Double Riichi" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
                           & vHand.handRiichi .~ DoubleRiichi
        snd (getYaku vi) @?= [Yaku 2 "Double riichi"]

    , testCase "Ippatsu" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
                           & vHand.handRiichi .~ Riichi
                           & vHand.handIppatsu .~ True
        sort (snd (getYaku vi)) @?= sort [Yaku 1 "Riichi", Yaku 1 "Ippatsu"]

    , testCase "Houtei Raoyui" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handPicks .~ [AgariCall "M1" Nan]
                           & vHand.handCalled .~ [Mentsu Shuntsu "P2" (Just (Shout{shoutKind = Chi}))]
                           & vKyoku.pWallTilesLeft .~ 0
        snd (getYaku vi) @?= [Yaku 1 "Houtei Raoyui"]

    , testCase "Haitei Raoyui" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handPicks .~ [AgariTsumo "M1"]
                           & vHand.handCalled .~ [Mentsu Shuntsu "P2" (Just (Shout{shoutKind = Chi}))]
                           & vKyoku.pWallTilesLeft .~ 0
        snd (getYaku vi) @?= [Yaku 1 "Haitei Raoyui"]

    , testCase "Rinshan Kaihou" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M1", "S5", "S6", "S7", "S2", "S3", "S4", "S8", "S8"]
                           & vHand.handPicks .~ [AgariTsumoWanpai "M1"] -- TODO Rinshan its own Tsumo?
                           & vHand.handCalled .~ [Mentsu Shuntsu "P2" (Just (Shout{shoutKind = Chi}))]
        snd (getYaku vi) @?= [Yaku 1 "Rinshan Kaihou"]

    , testCase "Dora and ura-dora are counted when calculating hand" $ do
        let vi = valueInfo & vHand.handConcealed._Wrapped .~ ["M1", "M2", "P2", "P3", "P4", "S5", "S6", "S7", "S1", "S2", "S3", "S9", "S9"]
                           & vHand.handPicks .~ [AgariCall "M3" Nan]
                           & vHand.handRiichi .~ Riichi
                           & vKyoku.pDora .~ ["S1"]
                           & vKyoku.pFlags .~ setFromList [OpenedUraDora ["M1"]]

        snd (getYaku vi) @?= [Yaku 1 "Riichi", YakuExtra 1 "Dora", YakuExtra 1 "Ura-Dora"]

    -- NOTE: implemented in Mechanics test, for now
    -- , testCase "Nagashi Mangan" $ do
    -- , testCase "Chankan" $ do error "test not implemented (here) (yet)"

    ]
