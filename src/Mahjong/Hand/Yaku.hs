------------------------------------------------------------------------------
-- | 
-- Module         : Mahjong.Yaku
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Hand.Yaku
    ( Yaku(..)

    -- * Check
    , ValueInfo(..), runYakuCheck

    -- * Available yaku
    , allStandard
    ) where

import Mahjong.Hand.Yaku.Builder
import Mahjong.Hand.Yaku.Standard

allStandard :: [YakuCheck Yaku]
allStandard = 
    [ chankan, chanta, chiitoitsu, chinitsu, doubleRiichi, fanpai
    , honitsu, honroutou, houteiRaoyui, iipeikou, ippatsu, ittsuu
    , junchan, kuitan, menzenTsumo, pinfu, riichi, rinshanKaihou
    , ryanpeikou, sanAnkou, sanKantsu, sanshokuDoujin, sanshokuDoukou
    , shouSangen, tanyao, toitoi, nagashiMangan ]
