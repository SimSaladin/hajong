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
    ( checkYaku, runYakuCheck, allStandard) where

------------------------------------------------------------------------------
import            Import
import            Control.Monad (msum)
------------------------------------------------------------------------------
import            Mahjong.Hand.Algo (Grouping)
import            Mahjong.Hand.Yaku.Builder
import            Mahjong.Hand.Yaku.Standard
------------------------------------------------------------------------------
import            Mahjong.Kyoku.Internal
------------------------------------------------------------------------------

-- | All standard Yaku. Internal list consists of mutually exclusive yaku.
--
-- We try to specify as much as possible in the YakuCheck code so that
-- mutual exclusivity is necessary at this level only for some exceptions,
-- i.e. yaku that wholly include some other yaku (iipeikou and ryanpeikou).
allStandard :: [[YakuCheck Yaku]]
allStandard = 
    [ [ chankan ]
    , [ chanta ]
    , [ chinitsu ]
    , [ yakuhaiRoundWind ]
    , [ yakuhaiSeatWind ]
    , [ yakuhaiRed ]
    , [ yakuhaiGreen ]
    , [ yakuhaiWhite ]
    , [ honitsu ]
    , [ honroutou ]
    , [ haiteiRaoyui ]
    , [ houteiRaoyui ]
    , [ ryanpeikou, chiitoitsu, iipeikou ]
    , [ ippatsu ]
    , [ ittsuu ]
    , [ junchan ]
    , [ kuitan ]
    , [ menzenTsumo ]
    , [ pinfu ]
    , [ riichi ]
    , [ rinshanKaihou ]
    , [ sanAnkou ]
    , [ sanKantsu ]
    , [ sanshokuDoujin ]
    , [ sanshokuDoukou ]
    , [ shouSangen ]
    , [ tanyao ]
    , [ toitoi ]
    , [ nagashiMangan ]
    ]

checkYaku :: [[YakuCheck Yaku]] -> ValueInfo -> Grouping -> [Yaku]
checkYaku yakus vi grp = mapMaybe (msum . map (runYakuCheck vi grp)) yakus

