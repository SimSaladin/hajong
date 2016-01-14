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
import           Import
import           Mahjong.Kyoku.Internal
------------------------------------------------------------------------------
import           Mahjong.Hand.Algo
import qualified Mahjong.Hand.Fu as Fu
import qualified Mahjong.Hand.Yaku as Yaku
------------------------------------------------------------------------------

-- | The hand has to have at least one valid grouping.
getValue :: ValueInfo -> Value
getValue vi = Value resYaku resFu (Fu.hanSum resYaku) points named
  where
    groupings = filter complete $ getGroupings vi

    possibleYakuCombos :: [(Grouping, [Yaku])]
    possibleYakuCombos = map (\grp -> (grp, Yaku.standardCheck vi grp)) groupings

    withFu :: [(Grouping, [Yaku], Fu)]
    withFu = do
        (grp, yaku) <- possibleYakuCombos
        (yaku', fu) <- Fu.grpFu grp yaku vi
        return (grp, yaku', fu)

    
    withMaxHan =
        maximumByMay (comparing (^._2.to Fu.hanSum)) withFu

    (resGrp, resYaku, resFu) = fromMaybe (headEx groupings, [], 0) withMaxHan
    (points, named) = valued resYaku resFu

-- ** Meta

-- | Calculate basic points or apply limit.
valued :: [Yaku] -> Fu -> (Points, Maybe Text)
valued yaku fu
    | han <= 4  = if' (basic >= 2000) mangan (basic, Nothing)
    | han == 5  = mangan
    | han <= 7  = haneman
    | han <= 10 = baiman
    | han <= 12 = sanbaiman
    | anyOf (each.yHan) (== 13) yaku = yakuman
    | otherwise = kazoeYakuman
  where
    han          = Fu.hanSum yaku
    basic        = fu * (2 ^ (2 + han))
    mangan       = (2000, Just "Mangan")
    haneman      = (3000, Just "Haneman")
    baiman       = (4000, Just "Baiman")
    sanbaiman    = (6000, Just "Sanbaiman")
    kazoeYakuman = (8000, Just "Kazoe-yakuman")
    yakuman      = (8000, Just "Yakuman")

