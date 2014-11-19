------------------------------------------------------------------------------
-- | 
-- Module         : Mahjong.Yaku
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Yaku
    ( Yaku(..)
    , runYakuCheck
    , allStandard
    ) where

import Mahjong.Hand
import Mahjong.Yaku.Builder
import Mahjong.Yaku.Standard

allStandard :: [YakuCheck Yaku]
allStandard = 
    [ chankan, chanta, chiitoitsu, chinitsu, doubleRiichi, fanpai
    , honitsu, honroutou, houteiRaoyui, iipeikou, ippatsu, ittsuu
    , junchan, kuitan, menzenTsumo, pinfu, riichi, rinshanKaihou
    , ryanpeikou, sanAnkou, sanKantsu, sanshokuDoujin, sanshokuDoukou
    , shouSangen, tanyao, toitoi, nagashiMangan ]
