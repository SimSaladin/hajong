------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Hand.Value
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Valueing (that's a word now) hands.
--
-- This should ideally calculate fu and yaku but that's out of scope for
-- now, so this module is used as a convenient abstraction for future.
------------------------------------------------------------------------------
module Mahjong.Hand.Value where

------------------------------------------------------------------------------
import            Import
import            Mahjong.Kyoku.Internal
import            Mahjong.Tiles
------------------------------------------------------------------------------
import            Mahjong.Hand.Internal
import            Mahjong.Hand.Yaku
import            Mahjong.Hand.Algo
import            Mahjong.Hand.Mentsu
------------------------------------------------------------------------------
import qualified  Data.List as L

-- ** Calculating

-- |
-- >>> import Mahjong.Configuration
-- >>> let kyoku = Kyoku {_pRound = (Ton, 1), _pTurn = Ton, _pOja = Player 3, _pFirstOja = Player 3, _pWallTilesLeft = 70, _pDora = ["P4"], _pFlags = mempty, _pPlayers = error "not used", _pHonba = 0, _pRiichi = 0, _pResults = Nothing, _pDeals = [], _sEvents = [], _sHands = error "not used", _sWall = [], _sWanpai = Wanpai [] [] ["M1"] [], _sWaiting = Nothing}
-- >>> getValue $ ValueInfo kyoku Ton $ Hand [] (map (\t -> Discard t Nothing False) ["N" ,"W" ,"G!","S7","P9","M6","S2","R!","S9","P1"]) Riichi False DrawNone [AgariTsumo "P2"] (return ["S5","P6","S6","P5","P4","S4","M5","S2","P3","P7","S7","S3","M5"]) (return NotFuriten) (return False) (return mempty)
-- Value {_vaYaku = [Yaku {_yHan = 1, _yName = "Menzen Tsumo"},Yaku {_yHan = 1, _yName = "Pinfu"},Yaku {_yHan = 1, _yName = "Riichi"},Yaku {_yHan = 1, _yName = "Tanyao"}], _vaFu = 20, _vaHan = 4, _vaValue = 1280, _vaNamed = Nothing}
getValue :: ValueInfo -> Value
getValue vi = Value yaku fu han val name
  where
    (grp, yaku) = getYaku vi
    fu          = getFu grp yaku vi
    han         = (each.yHan) `sumOf` yaku
    (val, name) = valued yaku han fu

-- | NOTE: we assume that the hand has at least one valid grouping.
getYaku :: ValueInfo -> (Grouping, [Yaku])
getYaku vi | null possibleYakuCombos = (headEx groupings, [])
           | otherwise               = maximumByEx (comparing $ hanSum . snd) possibleYakuCombos
  where groupings          = filter complete $ getGroupings vi
        possibleYakuCombos = map (\grp -> (grp, standardCheck vi grp)) groupings

hanSum :: [Yaku] -> Int
hanSum = sum . map _yHan

-- | Calculate fu points.
getFu :: Grouping -> [Yaku] -> ValueInfo -> Fu
getFu grp ys vi
    | hanSum ys >= 5 = 0
    | any (\y -> _yName y == "Chiitoitsu") ys = 25
    | any (\y -> _yName y == "Pinfu") ys = case vi^.vHand.handAgari of
                                                           Just AgariCall{} -> 30
                                                           _ -> 20
    | otherwise = rounded $ sum $ 20 : tsumoFu vi : waitFu grp vi : map tileGroupFu grp
    where rounded x = ceiling (fromIntegral x / 10) * 10

-- | +2 if tsumo.
tsumoFu :: ValueInfo -> Fu
tsumoFu vi | Just AgariTsumo{} <- vi^.vHand.handAgari = 2
           | otherwise                                = 0

-- | +10 if ron and hand is closed.
closedFu :: ValueInfo -> Fu
closedFu vi | [] <- vi^.vHand.handCalled
            , Just AgariCall{} <- vi^.vHand.handAgari = 10
            | otherwise                              = 0

waitFu :: Grouping -> ValueInfo -> Fu
waitFu grp vi = vi^.vHand.handAgari.to agariValue
    where
        agariValue Nothing = 0
        agariValue (Just (AgariCall Shout{..})) = getsValue shoutTile shoutTo

        agariValue (Just (AgariTsumo tile _)) =
            let possibleTileGroups = flip map grp $ \case
                    GroupWait _ ih _
                        | tile `elem` ih -> initEx ih
                    GroupComplete Mentsu{..}
                        | Nothing <- mentsuShout, tile `elem` mentsuTiles -> L.delete tile mentsuTiles
                        | Just Shout{..} <- mentsuShout, tile `elem` mentsuTiles, shoutKind `elem` [Ron, Chankan] -> L.delete tile mentsuTiles
                    _ -> []

            in maximumEx $ map (getsValue tile) possibleTileGroups

        getsValue t ih
            | [Honor honor] <- ih = case honor of
                Sangenpai _ -> 2 -- sangen pair
                Kazehai k | k == vi^.vPlayer || k == vi^.vKyoku.pRound._1 -> 2 -- value kaze pair (max 2 fu)
                _ -> 0
            | [a, b] <- ih
            , a ==~ b {- shanpon -}
                || tileNumber t `elem` [Just San, Just Chii] {- penchan -}
                || succMay a == predMay b || predMay a == succMay b {- kanchan -} = 2
            | otherwise = 0

tileGroupFu :: TileGroup -> Fu
tileGroupFu (GroupWait _ ih _) = mentsuFu $ Mentsu Jantou ih Nothing
tileGroupFu (GroupComplete mentsu) = mentsuFu mentsu
tileGroupFu _ = 0

mentsuFu :: Mentsu -> Fu
mentsuFu (Mentsu mk ts ms) = product [gokind mk, gotile (headEx ts), goshout ms]
  where
    gokind Koutsu = 2
    gokind Kantsu = 8
    gokind _      = 0

    gotile t | not (isSuited t) || isNothing (succMay t) || isNothing (predMay t) = 2
             | otherwise                                                          = 1

    goshout Nothing  = 2
    goshout (Just _) = 1

-- ** Meta

-- | Calculate basic points or apply limit.
valued :: [Yaku] -> Han -> Fu -> (Points, Maybe Text)
valued yaku han fu
    | han <= 4  = if' (basic >= 2000) mangan (basic, Nothing)
    | han == 5  = mangan
    | han <= 7  = haneman
    | han <= 10 = baiman
    | han <= 12 = sanbaiman
    | anyOf (each.yHan) (== 13) yaku = yakuman
    | otherwise = kazoeYakuman
  where basic        = fu * (2 ^ (2 + han))
        mangan       = (2000, Just "Mangan")
        haneman      = (3000, Just "Haneman")
        baiman       = (4000, Just "Baiman")
        sanbaiman    = (6000, Just "Sanbaiman")
        kazoeYakuman = (8000, Just "Kazoe-yakuman")
        yakuman      = (8000, Just "Yakuman")

