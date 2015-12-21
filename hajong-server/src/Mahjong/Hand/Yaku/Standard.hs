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
import           Mahjong.Tiles (Tile, Number(..), Kaze(..), kaze, tileNumber, (==~))
import           Mahjong.Hand.Mentsu (mentsuTiles)
import           Mahjong.Hand.Yaku.Builder
import           Mahjong.Hand.Algo (shantenBy, chiitoitsuShanten)
------------------------------------------------------------------------------
import           Mahjong.Kyoku.Internal
import           Mahjong.Kyoku.Flags
import           Mahjong.Hand.Internal
------------------------------------------------------------------------------

-- | All tiles in hand.
-- TODO move to Hand.hs
yakuAllTiles :: YakuCheck [Tile]
yakuAllTiles = do
    Hand{..} <- yakuState <&> view vHand
    return $ map pickedTile _handPicks ++
        runIdentity _handConcealed ++
        concatMap mentsuTiles _handCalled

-- * 4 mentsu + 1 jantou

-- ** Shuntsu based

pinfu :: YakuCheck Yaku
pinfu = do
    concealedHand
    anyJantou suited

    -- require: wait (at least) two-sided
    agari <- yakuState <&> pickedTile . view (vHand.handPicks.to lastEx)
    [a, _, c] <- anyShuntsu' (suited &. containsTile agari) <&> tileGroupTiles
    if (tileNumber a == Just Ii && agari == c) || (tileNumber c == Just Chuu && agari == a)
        then yakuFail
        else do
            replicateM_ 3 (anyShuntsu suited)
            return (Yaku 1 "Pinfu")

iipeikou :: YakuCheck Yaku
iipeikou = do
    concealedHand
    tile <- anyShuntsu' anyTile <&> tileGroupHead
    anyShuntsu (sameTile tile)
    return (Yaku 1 "Iipeikou")

ryanpeikou :: YakuCheck Yaku
ryanpeikou = concealedHand >> iipeikou >> iipeikou >> return (Yaku 3 "Ryanpeikou")

sanshokuDoujin :: YakuCheck Yaku
sanshokuDoujin = do
    concealedHandDegrade
    tile  <- anyShuntsu' anyTile <&> tileGroupHead
    tile' <- anyShuntsu' (f tile) <&> tileGroupHead
    anyShuntsu (f tile' &. f tile)
    return (Yaku 2 "Sanshoku Doujin")
    where
        f tile = sameNumber tile &. propNot (sameSuit tile)

ittsuu :: YakuCheck Yaku
ittsuu = do
    concealedHandDegrade
    tile <- anyShuntsu' (ofNumber Ii) <&> tileGroupHead
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

suuKantsu :: YakuCheck Yaku
suuKantsu = do
    replicateM_ 4 $ anyKantsu anyTile
    return (Yaku 13 "Suu Kantsu")

sanshokuDoukou :: YakuCheck Yaku
sanshokuDoukou = do
    tile <- anyKoutsuKantsu' anyTile <&> tileGroupHead
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

yakuhai :: Tile -> Text -> YakuCheck Yaku
yakuhai tile desc = do
    anyKoutsuKantsu (sameTile tile)
    return $ Yaku 1 desc

yakuhaiRoundWind, yakuhaiSeatWind :: YakuCheck Yaku
yakuhaiRoundWind = do
    info <- yakuState
    let roundTile = kaze $ info^.vKyoku.pRound._1
    yakuhai roundTile "Round Wind"

yakuhaiSeatWind = do
    info <- yakuState
    let playerKaze = kaze $ info^.vPlayer
    yakuhai playerKaze "Seat Wind"

yakuhaiRed, yakuhaiGreen, yakuhaiWhite :: YakuCheck Yaku
yakuhaiRed = yakuhai "R!" "Red"
yakuhaiGreen = yakuhai "G!" "Green"
yakuhaiWhite = yakuhai "W!" "White"

tanyao :: YakuCheck Yaku
tanyao = do
    concealedHand
    allMentsuOfKind (suited &. propNot terminal)
    return (Yaku 1 "Tanyao")

kuitan :: YakuCheck Yaku
kuitan = do
    openHand
    allMentsuOfKind (suited &. propNot terminal)
    return (Yaku 1 "Kuitan")

chanta :: YakuCheck Yaku
chanta = do
    concealedHandDegrade
    anyShuntsu terminal
    anyMentsuJantou honor
    replicateM_ 3 $ anyMentsuJantou (terminal |. honor)
    return (Yaku 2 "Chanta")

honitsu :: YakuCheck Yaku
honitsu = do
    concealedHandDegrade
    anyMentsuJantou honor
    tile <- anyMentsuJantou' suited <&> tileGroupHead
    replicateM_ 3 $ anyMentsuJantou (honor |. sameSuit tile)
    return (Yaku 3 "Honitsu")

junchan :: YakuCheck Yaku
junchan = do
    concealedHandDegrade
    anyShuntsu terminal 
    replicateM_ 4 $ anyMentsuJantou terminal
    return (Yaku 3 "Junchan")

chinitsu :: YakuCheck Yaku
chinitsu = do
    concealedHandDegrade
    tile <- anyMentsu' suited <&> tileGroupHead
    replicateM_ 3 (anyMentsu $ sameSuit tile)
    anyJantou (sameSuit tile)
    return (Yaku 6 "Chinitsu")

-- * Special

chiitoitsu :: YakuCheck Yaku
chiitoitsu = do
    concealedHand
    info <- yakuState
    if Just (-1) == shantenBy chiitoitsuShanten (info^.vHand.handConcealed._Wrapped ++ map pickedTile (info^.vHand.handPicks))
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

haiteiRaoyui :: YakuCheck Yaku
haiteiRaoyui = do
    info <- yakuState
    let tsumo = case info^.vHand.handPicks of
                     [AgariTsumo{}] -> True
                     _              -> False
    if info^.vKyoku.pWallTilesLeft == 0 && tsumo
        then return (Yaku 1 "Haitei Raoyui")
        else yakuFail

houteiRaoyui :: YakuCheck Yaku
houteiRaoyui = do
    info <- yakuState
    let called = case info^.vHand.handPicks of
                     [AgariCall{}] -> True
                     _             -> False
    if info^.vKyoku.pWallTilesLeft == 0 && called
        then return (Yaku 1 "Houtei Raoyui")
        else yakuFail

rinshanKaihou :: YakuCheck Yaku
rinshanKaihou = do
    info <- yakuState
    case info^?vHand.handPicks._last of
        Just AgariTsumoWanpai{} -> return (Yaku 1 "Rinshan Kaihou")
        _                       -> yakuFail

chankan :: YakuCheck Yaku
chankan = do
    info <- yakuState
    case info^?vHand.handPicks._last of
        Just AgariChankan{} -> return (Yaku 1 "Chankan")
        _                   -> yakuFail

nagashiMangan :: YakuCheck Yaku
nagashiMangan = yakuFail -- NOTE: this is implemented in @Mahjong.Kyoku@ currently.

-- | Dealer (tenhou) or non-dealer (chiihou) tsumo on first draw.
tenhouOrChiihou :: YakuCheck Yaku
tenhouOrChiihou = do
    requireFlag FirstRoundUninterrupted
    info <- yakuState
    case info^?vHand.handPicks._last of
        Just (AgariTsumo _) -> return $ Yaku 13 $ if info^.vPlayer == Ton then "Tenhou" else "Chiihou"
        _ -> yakuFail

-- | Non-dealer ron on first uninterrupted round.
renhou :: YakuCheck Yaku
renhou = do
    requireFlag FirstRoundUninterrupted
    undefined -- TODO This is not implemented yet.

countingDora :: YakuCheck Yaku
countingDora = do
    dora <- yakuState <&> view (vKyoku.pDora)
    tiles <- yakuAllTiles
    let num = length [ () | a <- dora, b <- tiles, a ==~ b ]
    case num of
        0 -> yakuFail
        _ -> return $ YakuExtra num "Dora"

countingUraDora :: YakuCheck Yaku
countingUraDora = do
    _ <- riichi
    [OpenedUraDora dora] <- yakuState <&> toListOf (vKyoku.pFlags._Wrapped.each.filtered isUraFlag)
    tiles <- yakuAllTiles
    let num = length [ () | a <- dora, b <- tiles, a ==~ b ]
    case num of
        0 -> yakuFail
        _ -> return $ YakuExtra num "Ura-Dora"
  where
    isUraFlag (OpenedUraDora _) = True
    isUraFlag _                 = False
