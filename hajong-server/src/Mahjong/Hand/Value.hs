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

import Mahjong.Hand.Yaku
import Mahjong.Hand.Mentsu
import Mahjong.Tiles

-- * Hand value

type Fu     = Int
type Han    = Int
type Points = Int

-- | Hand value
data Value = Value
    { _vaYaku  :: [Yaku]
    , _vaFu    :: Fu
    , _vaHan   :: Han
    , _vaValue :: Points
    , _vaNamed :: Maybe Text
    } deriving (Show, Read)

makeLenses ''Value

-- ** Calculating

getValue :: ValueInfo -> Value
getValue vi = traceShow vi $ Value yaku fu han val name
  where
    yaku        = getYaku vi
    fu          = getFu yaku vi
    han         = (each.to yakuHan) `sumOf` yaku
    (val, name) = valued yaku han fu

getYaku :: ValueInfo -> [Yaku]
getYaku vi = mapMaybe (runYakuCheck vi) allStandard

-- | Calculate fu points.
getFu :: [Yaku] -> ValueInfo -> Fu
getFu ys
  | any (\y -> yakuName y == "Chiitoitsu") ys = const 25
  | otherwise = rounded . sum . sequence [ sum . map mentsuValue . vMentsu
                                         , waitValue, baseFu ]
    where rounded = (* 10) . fst . (`divMod` 10)

baseFu :: ValueInfo -> Fu
baseFu vi | Just _ <- vWinCalled vi = 30
          | otherwise               = 20

waitValue :: ValueInfo -> Fu
waitValue = go <$> vWinWith <*> map mentsuTiles . filter (not . mentsuShouted) . vMentsu
    where go t = fromMaybe 0 . maximumMay . map (waitFu t)

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

        gotile
            | not (isSuited t) ||
              isNothing (succMay t) || isNothing (predMay t)
                        = 2
            | otherwise = 1

        goshout Nothing  = 2
        goshout (Just _) = 1

-- ** Meta

-- | Calculate basic points.
valued :: [Yaku] -> Han -> Fu -> (Points, Maybe Text)
valued yaku han fu
    | han <= 4  = if' (basic >= 2000) mangan (basic, Nothing)
    | han == 5  = mangan
    | han <= 7  = haneman
    | han <= 10 = baiman
    | han <= 12 = sanbaiman
    | anyOf (each.to yakuHan) (== 13) yaku = yakuman
    | otherwise = kazoeYakuman
  where basic        = fu * (2 ^ (2 + han))
        mangan       = (2000, Just "Mangan")
        haneman      = (3000, Just "Haneman")
        baiman       = (4000, Just "Baiman")
        sanbaiman    = (6000, Just "Sanbaiman")
        kazoeYakuman = (8000, Just "Kazoe-yakuman")
        yakuman      = (8000, Just "Yakuman")
