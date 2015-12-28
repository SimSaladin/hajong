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

-- ** Calculating

-- |
-- >>> import Mahjong.Configuration
-- >>> let kyoku = Kyoku {_pRound = (Ton, 1), _pTurn = Ton, _pOja = Player 3, _pFirstOja = Player 3, _pWallTilesLeft = 70, _pDora = ["P4"], _pFlags = mempty, _pPlayers = error "not used", _pHonba = 0, _pRiichi = 0, _pResults = Nothing, _pDeals = [], _sEvents = [], _sHands = error "not used", _sWall = [], _sWanpai = Wanpai [] [] ["M1"] [], _sWaiting = Nothing}
-- >>> getValue $ ValueInfo kyoku Ton $ Hand [] (map (\t -> Discard t Nothing False) ["N" ,"W" ,"G!","S7","P9","M6","S2","R!","S9","P1"]) Riichi False DrawNone [AgariTsumo "P2"] (return ["S5","P6","S6","P5","P4","S4","M5","S2","P3","P7","S7","S3","M5"]) (return NotFuriten) (return False) (return mempty)
-- Value {_vaYaku = [Yaku {_yHan = 1, _yName = "Menzen Tsumo"},Yaku {_yHan = 1, _yName = "Pinfu"},Yaku {_yHan = 1, _yName = "Riichi"},Yaku {_yHan = 1, _yName = "Tanyao"}], _vaFu = 20, _vaHan = 4, _vaValue = 1280, _vaNamed = Nothing}
getValue :: ValueInfo -> Value
getValue vi = Value yaku fu han val name
  where
    (_, yaku)   = getYaku vi -- TODO save grouping?
    fu          = getFu yaku vi
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
getFu :: [Yaku] -> ValueInfo -> Fu
getFu ys | any (\y -> _yName y == "Chiitoitsu") ys = const 25
         | otherwise = rounded . sum . sequence
             [ sum . map mentsuValue . view (vHand.handCalled)
             , waitValue
             , baseFu ]
    where rounded = (* 10) . fst . (`divMod` 10)

baseFu :: ValueInfo -> Fu
baseFu vi | AgariCall{} <- vi^?!vHand.handPicks._last = 30
          | otherwise                                 = 20

waitValue :: ValueInfo -> Fu
waitValue = go <$> pickedTile . (^?! vHand.handPicks._last) <*> map mentsuTiles . filter (not . mentsuShouted) . view (vHand.handCalled)
    where go tile = fromMaybe 0 . maximumMay . map (waitFu tile)

waitFu :: Tile -> [Tile] -> Fu
waitFu t xs = case xs of
    [a, _]    | a == t -> 2
    [a, b, c] | t == b -> 2
              | tileNumber a == Just Ii   && t == c -> 2
              | tileNumber c == Just Chuu && t == a -> 2
    _ -> 0

mentsuValue :: Mentsu -> Fu
mentsuValue (Mentsu mk t ms) = product [gokind mk, gotile, goshout ms]
  where
    gokind Koutsu = 2
    gokind Kantsu = 8
    gokind _      = 0

    gotile | not (isSuited t) || isNothing (succMay t) || isNothing (predMay t) = 2
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

