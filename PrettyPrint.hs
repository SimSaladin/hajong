{-# LANGUAGE FlexibleInstances #-}
module PrettyPrint where

import ClassyPrelude
import Control.Lens
import Tiles
import Riichi
import Data.Text (splitOn, chunksOf)

class PrettyPrint x where
    pshow :: x -> Text

class PrettyRead x where
    pread :: Text -> x

-- Player

instance PrettyPrint RiichiPlayer where
    pshow player = pshow (_riichiHand player)

-- Hand
instance PrettyPrint Hand where
    pshow hand = view handConcealed hand & unwords . map pshow

instance PrettyRead Hand where
    pread = initHand . pread

-- Tile
instance PrettyPrint Tile where
    pshow (Man n aka) = (if aka then "m" else "M") <> pshow n
    pshow (Pin n aka) = (if aka then "p" else "P") <> pshow n
    pshow (Sou n aka) = (if aka then "s" else "S") <> pshow n
    pshow (Sangen sangen) = case sangen of
                                Haku    -> "W!"
                                Hatsu   -> "G!"
                                Chun    -> "R!"
    pshow (Kaze kaze) = case kaze of
                            Ton     -> "E "
                            Nan     -> "S "
                            Shaa    -> "W "
                            Pei     -> "N "

instance PrettyRead Tile where
    pread xs = case m of
        "G" -> Sangen Hatsu
        "R" -> Sangen Chun
        "E" -> Kaze Ton

        "W" | n == "!"  -> Sangen Haku
            | otherwise -> Kaze Shaa

        "N" -> Kaze Pei
        "M" -> Man ( pread n) False
        "P" -> Pin ( pread n) False

        "S" | n == " "  -> Kaze Nan
            | otherwise -> Sou ( pread n) False

        _ -> error "no PrettyRead"
        where
            [m,n] = chunksOf 1 xs

instance PrettyRead [Tile] where
    pread = map pread . splitOn " "

-- Number

instance PrettyRead Number where
    pread = toEnum . (\x -> x - 1) . fromMaybe (error "No read") . readMay

instance PrettyPrint Number where
    pshow n = tshow (fromEnum n + 1)

-- Mensu

instance PrettyRead Mentsu where
    pread input = case pread <$> splitOn "-" input of
        tiles@(x:y:_)
            | length tiles == 4           -> Kantsu tiles True
            | length tiles == 3 && x == y -> Koutsu tiles True
            | length tiles == 3           -> Shuntsu tiles True
            | length tiles == 2 && x == y -> Jantou tiles True
            | otherwise -> error "pread: no parse"
        _ -> error "pread: no parse"

instance PrettyPrint Mentsu where
    pshow (Kantsu tiles _)  = intercalate "-" $ map pshow tiles
    pshow (Koutsu tiles _)  = intercalate "-" $ map pshow tiles
    pshow (Shuntsu tiles _) = intercalate "-" $ map pshow tiles
    pshow (Jantou tiles _)  = intercalate "-" $ map pshow tiles

