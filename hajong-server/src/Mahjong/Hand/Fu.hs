{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Mahjong.Hand.Fu
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2016-01-13T23:02:35+0200
------------------------------------------------------------------------------
module Mahjong.Hand.Fu where

import Import
import Mahjong.Kyoku.Internal
import Mahjong.Tiles
------------------------------------------------------------------------------
import Mahjong.Hand.Algo
import Mahjong.Hand.Internal
import Mahjong.Hand.Mentsu
------------------------------------------------------------------------------
import qualified Data.List as List

hanSum :: [Yaku] -> Int
hanSum = sum . map _yHan

-- | Calculate fu points.
grpFu :: Grouping -> [Yaku] -> ValueInfo -> [([Yaku], Fu)]
grpFu grp yaku vi
    | hanSum yaku >= 5 = [(yaku, 0)]
    | any (\y -> _yName y == "Chiitoitsu") yaku = [(yaku, 25)]
    | otherwise        = over (each._2) rounded
        [ (yaku', baseMentsuFu + wFu + openPinfuFu + tsumoFu)
        | wFu <- maybe [0] (waitFuAgari grp) (vi^.vHand.handAgari)
        , let allowsPinfu = mentsuFu' + wFu == 0
              openPinfuFu = if' (handOpen && allowsPinfu) 2 0
              yaku'       = if' allowsPinfu yaku yakuWithoutPinfu
        ]
  where
    baseMentsuFu = baseFu yaku vi + mentsuFu'
    mentsuFu' = mentsuFuGrouping valuedKaze grp

    tsumoFu = if any (\y -> _yName y == "Pinfu") yaku then 0 else 2

    yakuWithoutPinfu = List.deleteBy (equating _yName) Yaku { _yName = "Pinfu" } yaku

    handOpen = vi^.vHand.to isOpen

    valuedKaze = [ vi^.vPlayer, vi^.vKyoku.pRound._1 ]

    rounded x = ceiling (fromIntegral x / 10) * 10

-- | 30 if concealed ron, 20 otherwise (tsumo or open).
--
-- Note: result for chiitoi is probably something weird.
baseFu :: [Yaku] -> ValueInfo -> Fu
baseFu yaku vi
    | vi^.vHand.to isConcealed, Just AgariCall{} <- vi^.vHand.handAgari = 30
    | otherwise                                                         = 20

-- | We need the @Grouping@ (assumed complete) of the hand because @Agari@
-- doesn't save the target mentsu in the tsumo case.
--
-- NOTE: the return type is a list.  It is possible for the same wait
-- pattern to generate both 0 and 2 minipoints (consider 56789 and winning
-- with 7). Often one wants the most minipoints and chooses to take the 2,
-- but if the hand would value more as a Pinfu then 0 is the rational
-- choice.
waitFuAgari :: Grouping -> Agari -> [Fu]
waitFuAgari _   (AgariCall Shout{..}) = [ waitFu shoutTile shoutTo ]
waitFuAgari grp (AgariTsumo tile _)   = List.nub $ map (waitFu tile) possibleTileGroups
  -- because AgariTsumo doesn't have information about which mentsu it
  -- completes, we need to iterate through the possibilities. The
  -- @maximum@ above is not strictly necessary; if it is possible
  where
    possibleTileGroups = flip map grp $ \case
        GroupWait _ ih _ ->
            if' (tile `elem` ih) [tile] [] -- a pair wait
        GroupComplete Mentsu{..} ->
            case mentsuShout of
                Nothing        -> if' (tile `elem` mentsuTiles) (List.delete tile mentsuTiles) []
                Just Shout{..} -> if' (tile `elem` mentsuTiles && shoutKind `elem` [Ron, Chankan])
                                      (List.delete tile mentsuTiles) []
        _ -> []

-- |
-- >>> waitFu "M1" ["M1"]
-- 2
-- >>> waitFu "M1" ["M1", "M1"]
-- 2
-- >>> waitFu "M3" ["M1", "M2"]
-- 2
-- >>> waitFu "M7" ["M8", "M9"]
-- 2
-- >>> waitFu "M5" ["M4", "M6"]
-- 2
-- >>> waitFu "M5" ["M6", "M4"]
-- 2
-- >>> waitFu "M5" ["M4", "M3"]
-- 0
waitFu
    :: Tile   -- ^ Tile won with (agari)
    -> [Tile] -- ^ Tiles that waited for the agari
    -> Fu
waitFu agari_t agari_to = case agari_to of
    [_] -> 2 -- single wait
    [a, b]
        | a ==~ b                                          -> 2 {- shanpon -}
        | tileNumber agari_t `elem` [Just San, Just Chii]  -> 2 {- penchan -}
        | succMay a == predMay b || predMay a == succMay b -> 2 {- kanchan -}
    _ -> 0

mentsuFuGrouping
    :: [Kaze] -- ^ Valued kaze
    -> Grouping -> Fu
mentsuFuGrouping valuedKaze = sum . map (mentsuFuTileGroup valuedKaze)

-- | @TileGroup@ may be a mentsu (or pair), so it can yield Fu too.
mentsuFuTileGroup
    :: [Kaze] -- ^ Valued kaze
    -> TileGroup -> Fu
mentsuFuTileGroup valuedKaze (GroupWait _ ih _)     = mentsuFu valuedKaze $ Mentsu Jantou ih Nothing
mentsuFuTileGroup valuedKaze (GroupComplete mentsu) = mentsuFu valuedKaze mentsu
mentsuFuTileGroup _          _                      = 0

-- | Note: Values jantou as well.
-- >>> mentsuFu [Ton, Shaa] (jantou "M1")
-- 0
-- >>> mentsuFu [Ton, Shaa] (jantou "E")
-- 2
-- >>> mentsuFu [Ton] (jantou "R!")
-- 2
--
-- >>> mentsuFu [Shaa, Shaa] (jantou "W")
-- 4
--
-- >>> mentsuFu [Ton, Nan] (shuntsu "M1")
-- 0
--
-- >>> mentsuFu [Ton, Nan] (koutsu "M1")
-- 8
-- >>> mentsuFu [Ton, Nan] (calledPon "M1")
-- 4
-- >>> mentsuFu [Ton, Nan] (calledPon "M5")
-- 2
--
-- >>> mentsuFu [Ton, Nan] (kantsu "M1")
-- 32
-- >>> mentsuFu [Ton, Nan] (calledKan "R!")
-- 16
-- >>> mentsuFu [Ton, Nan] (calledKan "M5")
-- 8
mentsuFu
    :: [Kaze] -- ^ Valued kaze
    -> Mentsu -> Fu
mentsuFu valuedKaze (Mentsu mk ts ms) = case mk of
    Koutsu  -> 2 * ofTiles * ofClosed
    Kantsu  -> 8 * ofTiles * ofClosed
    Shuntsu -> 0
    Jantou  -> case tile of
        Suited{}          -> 0
        Honor Sangenpai{} -> 2
        Honor (Kazehai k) -> sum [ if' (k == k') 2 0 | k' <- valuedKaze ]
  where
    tile     = headEx ts

    ofTiles  = if' (terminal tile || honor tile) 2 1
    ofClosed = if' (isNothing ms) 2 1
