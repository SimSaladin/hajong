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
    ( standardCheck, checkYaku, runYakuCheck) where

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
standardYaku :: [[YakuCheck Yaku]]
standardYaku = 
    [ [ chankan ]
    , [ menzenTsumo ]
    , [ haiteiRaoyui ]
    , [ houteiRaoyui ]
    , [ ippatsu ]
    , [ riichi ]
    , [ rinshanKaihou ]
    , [ nagashiMangan ]

    , [ yakuhaiRoundWind ]
    , [ yakuhaiSeatWind ]
    , [ yakuhaiRed ]
    , [ yakuhaiGreen ]
    , [ yakuhaiWhite ]
    , [ shouSangen ]

    , [ chanta ]
    , [ ryanpeikou, chiitoitsu, iipeikou ]
    , [ ittsuu ]
    , [ pinfu ]

    , [ chinitsu ]
    , [ honitsu ]
    , [ honroutou ]
    , [ junchan ]

    , [ sanAnkou ]
    , [ sanKantsu ]
    , [ sanshokuDoujin ]
    , [ sanshokuDoukou ]
    , [ tanyao ]
    , [ kuitan ]
    , [ toitoi ]

    , [ countingDora ]
    , [ countingUraDora ]
    ]

standardYakumans :: [YakuCheck Yaku]
standardYakumans = [ kokushiMusou, daisangen, suuankou, suushiihou, tsuuiisou, ryuuiisou, chinroutou, chuurenPoutou, suuKantsu, tenhouOrChiihou ]

-- | Check a list of yaku that are 
checkYaku :: [[YakuCheck Yaku]] -> ValueInfo -> Grouping -> [Yaku]
checkYaku yakus vi grp = mapMaybe (msum . map (runYakuCheck vi grp)) yakus

-- | The "standard" checker. If there are yakuman, then yield it or them.
-- Otherwise resolve the normal yaku. If normal yaku add up to a Kazoe then
-- yield Kazoe Yakuman. Otherwise return all matched yaku.
standardCheck :: ValueInfo -> Grouping -> [Yaku]
standardCheck vi grp
    | yakumans <- mapMaybe (runYakuCheck vi grp) standardYakumans, not (null yakumans) = yakumans
    | yaku <- checkYaku standardYaku vi grp = if sumOf (each.yHan) yaku >= 13 then [Yaku 13 "Kazoe Yakuman"] else yaku
