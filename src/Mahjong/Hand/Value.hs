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
    , _vaValue :: Int
    , _vaNamed :: Maybe Text
    } deriving (Show, Read)

makeLenses ''Value

-- ** Calculating

getValue :: ValueInfo -> Value
getValue vi = Value yaku fu han val name
  where
    yaku        = getYaku vi
    fu          = getFu yaku vi
    han         = (each.to yakuHan) `sumOf` yaku
    (val, name) = han `valued` fu

getYaku :: ValueInfo -> [Yaku]
getYaku vi = mapMaybe (runYakuCheck vi) allStandard

-- | Calculate fu points.
getFu :: [Yaku] -> ValueInfo -> Fu
getFu ys
  | any (\y -> yakuName y == "Chiitoitsu") ys = const 25
  | otherwise = rounded . liftA2 (+) (sum . map mentsuValue . vMentsu) waitValue
    where rounded = (* 10) . fst . (`divMod` 10)

waitValue :: ValueInfo -> Fu
waitValue = go <$> vWinWith <*> map mentsuTiles . filter (not . mentsuShouted) . vMentsu
    where
        go t = fromMaybe 0 . maximumMay . map (waitFu t)

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

valued :: Han -> Fu -> (Points, Maybe Text)
valued yaku fu = undefined
