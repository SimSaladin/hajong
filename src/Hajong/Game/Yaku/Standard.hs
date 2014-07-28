------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Yaku.Standard
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.Yaku.Standard where

import ClassyPrelude

import Hajong.Game.Types
import Hajong.Game.Tiles
import Hajong.Game.Yaku.Builder

-- * 4 mentsu + 1 jantou

-- ** Shuntsu based

yakuPinfu :: Yaku Int
yakuPinfu = do
    concealedHand
    replicateM_ 4 (anyShuntsu suited)
    anyJantou suited
    return 1

yakuIipeikou :: Yaku Int
yakuIipeikou = do
    concealedHand
    tile <- anyShuntsu' anyTile
    anyShuntsu (sameTile tile)
    return 1

yakuRyanpeikou :: Yaku Int
yakuRyanpeikou = concealedHand >> yakuIipeikou >> yakuIipeikou >> return 3

yakuSanshokuDoujin :: Yaku Int
yakuSanshokuDoujin = do
    concealedHandDegrade
    tile  <- anyShuntsu' anyTile
    tile' <- anyShuntsu' (f tile)
    anyShuntsu (f tile' &. f tile)
    return 2
    where
        f tile = sameNumber tile &. propNot (sameSuit tile)

yakuIttsuu :: Yaku Int
yakuIttsuu = do
    concealedHandDegrade
    tile <- anyShuntsu' (ofNumber Ii)
    anyShuntsu (sameSuit tile &. ofNumber Suu)
    anyShuntsu (sameSuit tile &. ofNumber Chii)
    return 2

-- ** Koutsu/kantsu based

-- NOTE this does not combine with chanta
yakuHonroutou :: Yaku Int
yakuHonroutou = do
    replicateM_ 4 $ anyKoutsuKantsu (terminal |. honor) 
    anyJantou (terminal |. honor)
    return 2

yakuToitoi :: Yaku Int
yakuToitoi = do
    replicateM_ 4 $ anyKoutsuKantsu anyTile
    return 2

yakuSanankou :: Yaku Int
yakuSanankou = do
    replicateM_ 3 $ anyKoutsuKantsu concealed
    return 2

yakuSanKantsu :: Yaku Int
yakuSanKantsu = do
    replicateM_ 3 $ anyKantsu anyTile
    return 2

yakuSanshokuDoukou :: Yaku Int
yakuSanshokuDoukou = do
    tile <- anyKoutsuKantsu' anyTile
    replicateM_ 2 $ anyKoutsuKantsu (sameNumber tile)
    return 2

yakuShouSangen :: Yaku Int
yakuShouSangen = do
    anyKoutsuKantsu sangenpai
    anyKoutsuKantsu sangenpai
    anyJantou sangenpai
    anyMentsu (propNot sangenpai)
    return 2

-- ** Tile kind based

yakuFanpai :: Yaku Int
yakuFanpai = do
    info <- yakuState
    let roundTile = Kaze $ yakuRoundKaze info
        playerKaze = Kaze $ yakuPlayerKaze info
    tile <- anyMentsu' (sangenpai |. sameTile roundTile |. sameTile playerKaze)
    return $ if roundTile == playerKaze && roundTile == tile
        then 2
        else 1

yakuTanyao :: Yaku Int
yakuTanyao = do
    concealedHand
    allMentsuOfKind suited
    return 1

yakuKuitan :: Yaku Int
yakuKuitan = do
    openHand
    allMentsuOfKind suited
    return 1

yakuChanta :: Yaku Int
yakuChanta = do -- TODO this does not notice 7-8-9 Shuntsu!
    anyShuntsu terminal
    replicateM_ 4 $ anyMentsuJantou (terminal |. honor)
    return 2

yakuHonitsu :: Yaku Int
yakuHonitsu = do
    concealedHandDegrade
    anyMentsuJantou honor
    tile <- anyMentsuJantou' suited
    replicateM_ 3 $ anyMentsuJantou (honor |. sameSuit tile)
    return 3

yakuJunchan :: Yaku Int
yakuJunchan = do
    concealedHandDegrade
    allMentsuOfKind terminal -- TODO this does not notice 7-8-9 shuntsu
    return 3

yakuChinitsu :: Yaku Int
yakuChinitsu = do
    concealedHandDegrade
    tile <- anyMentsu' suited
    replicateM_ 3 (anyMentsu $ sameSuit tile)
    anyJantou (sameSuit tile)
    return 6

-- * Special

yakuChiitoitsu :: Yaku ()
yakuChiitoitsu = undefined -- TODO how does this implement?

-- * Unrelated to mentsu

yakuMenzenTsumo :: Yaku ()
yakuMenzenTsumo = undefined

yakuRiichi :: Yaku ()
yakuRiichi = undefined

yakuIppatsu :: Yaku ()
yakuIppatsu = undefined

yakuDoubleRiichi :: Yaku ()
yakuDoubleRiichi = undefined

yakuHouteiRaoyui :: Yaku ()
yakuHouteiRaoyui = undefined

yakuRinshanKaihou :: Yaku ()
yakuRinshanKaihou = undefined

yakuChankan :: Yaku ()
yakuChankan = undefined

yakuNagashiMangan :: Yaku ()
yakuNagashiMangan = undefined
