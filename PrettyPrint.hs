{-# LANGUAGE FlexibleInstances #-}
module PrettyPrint where

import ClassyPrelude
import Control.Lens
import Tiles
import Riichi
import Data.List (transpose)
import Data.Text (splitOn, chunksOf, justifyLeft, justifyRight)
import qualified Data.List.Split as L (chunksOf)

class PrettyPrint x where
    pshow :: x -> Text

class PrettyRead x where
    pread :: Text -> x

-- Discard pile position
newtype DiscardPileOwn   = DiscardPileOwn [Tile] deriving (Show, Read, Eq)
newtype DiscardPileLeft  = DiscardPileLeft [Tile] deriving (Show, Read, Eq)
newtype DiscardPileRight = DiscardPileRight [Tile] deriving (Show, Read, Eq)
newtype DiscardPileFront = DiscardPileFront [Tile] deriving (Show, Read, Eq)

-- Player

instance PrettyPrint RiichiPlayer where
    pshow = (\myhand public -> public <> "\n" <> myhand)
        <$> view (riichiHand.to pshow)
        <*> view (riichiPublic.to pshow)

instance PrettyPrint RiichiPublic where
    pshow = 

-- Hand

-- | Own hand
instance PrettyPrint Hand where
    pshow = (\conc tsumo -> conc <> " : " <> tsumo)
        <$> view (handConcealed.to pshow)
        <*> view (handPick.to (maybe "" pshow))

-- Note: this is not really useful
instance PrettyRead Hand where
    pread = initHand . pread

-- Game

instance PrettyPrint DiscardPileOwn where
    pshow (DiscardPileOwn tiles) =
        intercalate "\n" $ map (justifyLeft (6 * 3 - 1) ' ' . unwords)
        $ discardSplit tiles

instance PrettyPrint DiscardPileLeft where
    pshow (DiscardPileLeft tiles) =
        intercalate "\n" $ map (justifyRight 8 ' ' . unwords)
        $ transpose $ reverse $ discardSplit tiles

instance PrettyPrint DiscardPileRight where
    pshow (DiscardPileRight tiles) =
        intercalate "\n" $ map (justifyLeft 8 ' ' . unwords)
        $ reverse $ transpose $ discardSplit tiles

instance PrettyPrint DiscardPileFront where
    pshow (DiscardPileFront tiles) =
        intercalate "\n" $ map (justifyRight (6 * 3 - 1) ' ' . unwords)
        $ reverse $ reverse <$> discardSplit tiles

-- | helper function for discard pretty printers
discardSplit :: [Tile] -> [[Text]]
discardSplit = (\(xs, x) -> xs ++ [x]) . over _2 join . splitAt 2 . L.chunksOf 6 . map pshow

-- Tile

instance PrettyPrint [Tile] where pshow = unwords . map pshow
instance PrettyRead [Tile]  where pread = map pread . words

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
            [m,n] = case chunksOf 1 xs of
                        [a,b] -> [a, b ]
                        [a]   -> [a," "]
                        _ -> error $ "Tile no read: " <> unpack xs

-- Number

instance PrettyRead Number where
    pread = toEnum . (\x -> x - 1) . fromMaybe (error "No read") . readMay

instance PrettyPrint Number where
    pshow n = tshow (fromEnum n + 1)

-- Mensu

instance PrettyPrint Mentsu where
    pshow (Kantsu tiles _)  = intercalate "-" $ map pshow tiles
    pshow (Koutsu tiles _)  = intercalate "-" $ map pshow tiles
    pshow (Shuntsu tiles _) = intercalate "-" $ map pshow tiles
    pshow (Jantou tiles _)  = intercalate "-" $ map pshow tiles
instance PrettyRead Mentsu where
    pread input = case pread <$> splitOn "-" input of
        tiles@(x:y:_)
            | length tiles == 4           -> Kantsu tiles True
            | length tiles == 3 && x == y -> Koutsu tiles True
            | length tiles == 3           -> Shuntsu tiles True
            | length tiles == 2 && x == y -> Jantou tiles True
            | otherwise -> error "pread: no parse"
        _ -> error "pread: no parse"
