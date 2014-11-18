------------------------------------------------------------------------------
-- | 
-- Module         : Mahjong.Hand.Value
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
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

import Mahjong.Yaku
import Mahjong.Hand
import Mahjong.Hand.Mentsu
import Mahjong.Tiles

getValue :: ValueInfo -> Value
getValue = Value <$> getYaku <*> (*10) . fst . (`divMod` 10) . getFu

getYaku :: ValueInfo -> [Yaku]
getYaku vi = mapMaybe (runYakuCheck vi) allStandard

-- | Calculate fu points. Not rounded.
getFu :: ValueInfo -> Fu
getFu = (+) <$> sum . map mentsuValue . vMentsu
            <*> waitValue

waitValue :: ValueInfo -> Fu
waitValue = undefined

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
