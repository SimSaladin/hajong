{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Yaku.Standard
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Hand.Yaku.Standard where

------------------------------------------------------------------------------
import           Import
import           Mahjong.Tiles (Tile, Number(..), kaze)
import           Mahjong.Hand.Yaku.Builder
import           Mahjong.Hand.Algo (shantenBy, chiitoitsuShanten)
------------------------------------------------------------------------------
import           Mahjong.Kyoku.Internal
import           Mahjong.Hand.Internal
------------------------------------------------------------------------------

-- * 4 mentsu + 1 jantou

-- ** Shuntsu based

pinfu :: YakuCheck Yaku
pinfu = do
    concealedHand
    replicateM_ 4 (anyShuntsu suited)
    anyJantou suited
    return (Yaku 1 "Pinfu")

iipeikou :: YakuCheck Yaku
iipeikou = do
    concealedHand
    tile <- anyShuntsu' anyTile
    anyShuntsu (sameTile tile)
    return (Yaku 1 "Iipeikou")

ryanpeikou :: YakuCheck Yaku
ryanpeikou = concealedHand >> iipeikou >> iipeikou >> return (Yaku 3 "Ryanpeikou")

sanshokuDoujin :: YakuCheck Yaku
sanshokuDoujin = do
    concealedHandDegrade
    tile  <- anyShuntsu' anyTile
    tile' <- anyShuntsu' (f tile)
    anyShuntsu (f tile' &. f tile)
    return (Yaku 2 "Sanshoku Doujin")
    where
        f tile = sameNumber tile &. propNot (sameSuit tile)

ittsuu :: YakuCheck Yaku
ittsuu = do
    concealedHandDegrade
    tile <- anyShuntsu' (ofNumber Ii)
    anyShuntsu (sameSuit tile &. ofNumber Suu)
    anyShuntsu (sameSuit tile &. ofNumber Chii)
    return (Yaku 2 "Ittsuu")

-- ** Koutsu/kantsu based

-- NOTE this does/should not combine with chanta
honroutou :: YakuCheck Yaku
honroutou = do
    replicateM_ 4 $ anyKoutsuKantsu (terminal |. honor) 
    anyJantou (terminal |. honor)
    return (Yaku 2 "Honroutou")

toitoi :: YakuCheck Yaku
toitoi = do
    replicateM_ 4 $ anyKoutsuKantsu anyTile
    return (Yaku 2 "Toitoi")

sanAnkou :: YakuCheck Yaku
sanAnkou = do
    replicateM_ 3 $ anyKoutsuKantsu concealed
    return (Yaku 2 "San ankou")

sanKantsu :: YakuCheck Yaku
sanKantsu = do
    replicateM_ 3 $ anyKantsu anyTile
    return (Yaku 2 "San kantsu")

sanshokuDoukou :: YakuCheck Yaku
sanshokuDoukou = do
    tile <- anyKoutsuKantsu' anyTile
    replicateM_ 2 $ anyKoutsuKantsu (sameNumber tile)
    return (Yaku 2 "San Shoku Doukou")

shouSangen :: YakuCheck Yaku
shouSangen = do
    anyKoutsuKantsu sangenpai
    anyKoutsuKantsu sangenpai
    anyJantou sangenpai
    anyMentsu (propNot sangenpai)
    return (Yaku 2 "Shou Sangen")

-- ** Tile kind based

fanpai :: YakuCheck Yaku
fanpai = do
    info <- yakuState
    let roundTile  = kaze $ info^.vKyoku.pRound
        playerKaze = kaze $ info^.vPlayer
    tile <- anyMentsu' (sangenpai |. sameTile roundTile |. sameTile playerKaze)
    return (Yaku
        (if roundTile == playerKaze && roundTile == tile then 2 else 1)
                                                         "Fanpai")

tanyao :: YakuCheck Yaku
tanyao = do
    concealedHand
    allMentsuOfKind suited
    return (Yaku 1 "Tanyao")

kuitan :: YakuCheck Yaku
kuitan = do
    openHand
    allMentsuOfKind suited
    return (Yaku 1 "Kuitan")

chanta :: YakuCheck Yaku
chanta = do -- TODO this does not notice 7-8-9 Shuntsu!
            -- FIXME this looks very bonken indeed
    anyShuntsu terminal
    replicateM_ 4 $ anyMentsuJantou (terminal |. honor)
    return (Yaku 2 "Chanta")

honitsu :: YakuCheck Yaku
honitsu = do
    concealedHandDegrade
    anyMentsuJantou honor
    tile <- anyMentsuJantou' suited
    replicateM_ 3 $ anyMentsuJantou (honor |. sameSuit tile)
    return (Yaku 3 "Honitsu")

junchan :: YakuCheck Yaku
junchan = do
    concealedHandDegrade
    allMentsuOfKind terminal -- TODO this does not notice 7-8-9 shuntsu
    return (Yaku 3 "Junchan")

chinitsu :: YakuCheck Yaku
chinitsu = do
    concealedHandDegrade
    tile <- anyMentsu' suited
    replicateM_ 3 (anyMentsu $ sameSuit tile)
    anyJantou (sameSuit tile)
    return (Yaku 6 "Chinitsu")

-- * Special

chiitoitsu :: YakuCheck Yaku
chiitoitsu = do
    concealedHand
    info <- yakuState
    if Just (-1) == shantenBy chiitoitsuShanten (info^.vHand.handConcealed._Wrapped :: [Tile])
        then return (Yaku 2 "Chiitoitsu")
        else yakuFail

-- * Unrelated to mentsu

menzenTsumo :: YakuCheck Yaku
menzenTsumo = do
    concealedHand
    info <- yakuState
    case info^?vHand.handPicks._last of
        Just (AgariTsumo _) -> return (Yaku 1 "Menzen Tsumo")
        _ -> yakuFail

riichi :: YakuCheck Yaku
riichi = do
    concealedHand
    info <- yakuState
    case info^.vHand.handRiichi of
        NoRiichi     -> yakuFail
        Riichi       -> return (Yaku 1 "Riichi")
        DoubleRiichi -> return (Yaku 2 "Double riichi")

ippatsu :: YakuCheck Yaku
ippatsu = do
    _    <- riichi
    info <- yakuState
    if info^.vHand.handIppatsu
        then return (Yaku 1 "Ippatsu")
        else yakuFail

houteiRaoyui :: YakuCheck Yaku
houteiRaoyui = do
    info <- yakuState
    if info^.vKyoku.pWallTilesLeft == 0
        then return (Yaku 1 "Houtei Raoyui")
        else yakuFail

rinshanKaihou :: YakuCheck Yaku
rinshanKaihou = do
    info <- yakuState
    case info^?vHand.handPicks._last of
        Just FromWanpai{} -> return (Yaku 1 "Rinshan Kaihou")
        _                 -> yakuFail

chankan :: YakuCheck Yaku
chankan = do
    info <- yakuState
    case info^?vHand.handPicks._last of
        Just AgariChankan{} -> return (Yaku 1 "Chankan")
        _                   -> yakuFail

nagashiMangan :: YakuCheck Yaku
nagashiMangan = yakuFail -- TODO Implement
