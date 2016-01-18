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

import qualified Data.List as L
------------------------------------------------------------------------------
import           Import
import           Mahjong.Tiles (Tile, Number(..), Kaze(..), kaze, tileNumber, (==~), succCirc)
import qualified Mahjong.Tiles as T
import qualified Mahjong.Hand.Mentsu as M
import           Mahjong.Hand.Mentsu (mentsuTiles)
import           Mahjong.Hand.Yaku.Builder
import           Mahjong.Hand.Algo (shantenBy, chiitoitsuShanten)
import           Mahjong.Kyoku.Flags
------------------------------------------------------------------------------
import           Mahjong.Kyoku.Internal
import           Mahjong.Hand.Internal
------------------------------------------------------------------------------

-- | All tiles in hand.
-- TODO move to Hand.hs
yakuAllTiles :: YakuCheck [Tile]
yakuAllTiles = do
    Hand{..} <- yakuState <&> view vHand
    return $ _handConcealed
        ++ map pickedTile _handPicks
        ++ concatMap mentsuTiles _handCalled
        ++ case _handAgari of
            Just (AgariTsumo tile _) -> [ tile ]
            Just (AgariCall shout) -> M.shoutTile shout : M.shoutTo shout
            _ -> []

-- * 4 mentsu + 1 jantou

-- ** Shuntsu based

pinfu :: YakuCheck Yaku
pinfu = do
    concealedHand
    anyJantou =<< valueless

    -- require: wait (at least) two-sided
    agari <- yakuState <&> view (vHand.handAgari)
    agari_t <- maybe yakuFail (return . agariTile) agari -- TODO: agari should always be present in ValueInfo

    [a, _, c] <- anyShuntsu' (suited &. containsTile agari_t) <&> tileGroupTiles
    if (tileNumber a == Just Ii && agari_t ==~ c) || (tileNumber c == Just Chuu && agari_t ==~ a)
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
    tiles <- yakuAllTiles
    info <- yakuState
    if Just (-1) == shantenBy chiitoitsuShanten tiles
        then return (Yaku 2 "Chiitoitsu")
        else yakuFail

-- | 13 orphans
kokushiMusou :: YakuCheck Yaku
kokushiMusou = do
    leftover <- yakuAllTiles <&> (L.\\ ["P1", "P9", "M1", "M9", "S1", "S9", "E", "S", "W", "N", "G", "R", "W!"])
    case leftover of
        [x] | T.honor x || T.terminal x -> return $ Yaku 13 "Kokushi Musou"
        _                               -> yakuFail

-- * Yakumans with standard composition

-- | all three dragon triplets
daisangen :: YakuCheck Yaku
daisangen = yakuhaiRed >> yakuhaiGreen >> yakuhaiWhite >> return (Yaku 13 "Daisangen")

-- | 4 concealed triplets
suuankou :: YakuCheck Yaku
suuankou = do
    replicateM_ 4 $ anyKoutsuKantsu concealed
    return (Yaku 13 "Suuankou")

-- | Either shousuushii (3 triplets and pair of winds) or daisuushii (4
-- triplets of winds).
suushiihou :: YakuCheck Yaku
suushiihou = do
    replicateM_ 3 $ anyKoutsuKantsu kazehai
    tg <- anyMentsuJantou' kazehai
    return $ if length (tileGroupTiles tg) == 2 then Yaku 13 "Shousuushii" else Yaku 13 "Daisuushii"

-- | Only honors
tsuuiisou :: YakuCheck Yaku
tsuuiisou = do
    replicateM_ 4 $ anyMentsu honor
    anyJantou honor
    return $ Yaku 13 "Tsuuiisou"

-- | all-green; green dragons and/or S2,3,4,6,8
ryuuiisou :: YakuCheck Yaku
ryuuiisou = do
    tiles <- yakuAllTiles
    if all isGreen tiles then return $ Yaku 13 "Ryyiisou" else yakuFail
  where
    isGreen t = any (t ==~) ["G", "S2", "S3", "S4", "S6", "S8"]

-- | all-terminals
chinroutou :: YakuCheck Yaku
chinroutou = do
    tiles <- yakuAllTiles
    if all T.terminal tiles then return $ Yaku 13 "Chinroutou" else yakuFail

-- | Nine gates; 1-1-1-2-3-4-5-6-7-8-9-9-9 + any other of the suit.
chuurenPoutou :: YakuCheck Yaku
chuurenPoutou = do
    tiles@(t:ts) <- yakuAllTiles
    let afterPattern = map (fromMaybe (error "not used") . tileNumber) tiles L.\\ ["1","1","1","2","3","4","5","6","7","8","9","9","9"]
    if all (T.suitedSame t) ts && length afterPattern == 1
        then return (Yaku 13 "Chuuren Poutou")
        else yakuFail

suuKantsu :: YakuCheck Yaku
suuKantsu = do
    replicateM_ 4 $ anyKantsu anyTile
    return (Yaku 13 "Suu Kantsu")

-- * Unrelated to mentsu

menzenTsumo :: YakuCheck Yaku
menzenTsumo = do
    concealedHand
    info <- yakuState
    case info^.vHand.handAgari of
        Just AgariTsumo{} -> return (Yaku 1 "Menzen Tsumo")
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
    let tsumo = case info^.vHand.handAgari of
                     Just AgariTsumo{} -> True
                     _                 -> False
    if info^.vKyoku.pWallTilesLeft == 0 && tsumo
        then return (Yaku 1 "Haitei Raoyui")
        else yakuFail

houteiRaoyui :: YakuCheck Yaku
houteiRaoyui = do
    info <- yakuState
    let called = case info^.vHand.handAgari of
                     Just AgariCall{} -> True
                     _                -> False
    if info^.vKyoku.pWallTilesLeft == 0 && called
        then return (Yaku 1 "Houtei Raoyui")
        else yakuFail

rinshanKaihou :: YakuCheck Yaku
rinshanKaihou = do
    info <- yakuState
    case info^.vHand.handAgari of
        Just (AgariTsumo _ True) -> return (Yaku 1 "Rinshan Kaihou")
        _                        -> yakuFail

chankan :: YakuCheck Yaku
chankan = do
    info <- yakuState
    case info^.vHand.handAgari of
        Just (AgariCall shout) | M.shoutKind shout == M.Chankan -> return (Yaku 1 "Chankan")
        _ -> yakuFail

nagashiMangan :: YakuCheck Yaku
nagashiMangan = yakuFail -- NOTE: this is implemented in @Mahjong.Kyoku@ currently.

-- | Dealer (tenhou) or non-dealer (chiihou) tsumo on first draw.
tenhouOrChiihou :: YakuCheck Yaku
tenhouOrChiihou = do
    requireFlag FirstRoundUninterrupted
    info <- yakuState
    case info^.vHand.handAgari of
        Just AgariTsumo{} -> return $ Yaku 13 $ if info^.vPlayer == Ton then "Tenhou" else "Chiihou"
        _ -> yakuFail

-- | Non-dealer ron on first uninterrupted round.
renhou :: YakuCheck Yaku
renhou = do
    requireFlag FirstRoundUninterrupted
    error "Not implemented" -- TODO This is not implemented yet.

countingDora :: YakuCheck Yaku
countingDora = do
    dora <- yakuState <&> view (vKyoku.pDora)
    tiles <- yakuAllTiles
    let num = length [ () | a <- map succCirc dora, b <- tiles, a ==~ b ]
    case num of
        0 -> yakuFail
        _ -> return $ YakuExtra num "Dora"

countingUraDora :: YakuCheck Yaku
countingUraDora = do
    void $ riichi
    dora <- yakuState <&> concat . toListOf (vKyoku.pFlags._Wrapped.each.to getUra)
    tiles <- yakuAllTiles
    let num = length [ () | a <- map succCirc dora, b <- tiles, a ==~ T.TileEq b ]
    case num of
        0 -> yakuFail
        _ -> return $ YakuExtra num "Ura-Dora"
  where
    getUra (OpenedUraDora xs) = xs
    getUra _                  = []

countingAkaDora :: YakuCheck Yaku
countingAkaDora = do
    num <- yakuAllTiles <&> length . filter T.isAka
    case num of
        0 -> yakuFail
        _ -> return $ YakuExtra num "Aka-Dora"

