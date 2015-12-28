{-# LANGUAGE DefaultSignatures #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Tiles
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Mahjong tiles and relevant functions.
------------------------------------------------------------------------------
module Mahjong.Tiles where

import Prelude (Read(..), lex)
import Import hiding ((<>))
------------------------------------------------------------------------------
import Text.PrettyPrint.ANSI.Leijen ((<>), displayS, renderCompact)
import qualified Data.List as L
------------------------------------------------------------------------------

-- * Types

-- | A (japanese) mahjong tile.
data Tile = Suited TileKind Number Aka
          | Honor Honor
          deriving (Ord)

instance Eq Tile where
    a == b = a ==~ b

data TileKind = ManTile | PinTile | SouTile | HonorTile
              deriving (Show, Read, Eq, Ord)

-- | Is akadora?
type Aka = Bool

-- | The number of man, pin and sou tiles.
data Number = Ii | Ryan | San | Suu | Wu | Rou | Chii | Paa | Chuu
            deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Honor = Sangenpai Sangen
           | Kazehai Kaze
           deriving (Show, Read, Eq, Ord)

data Sangen = Haku | Hatsu | Chun
            deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Kaze = Ton | Nan | Shaa | Pei
          deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- Instances

-- Tile
instance IsString Tile where
    fromString [a]     = fromString [a, ' ']
    fromString [k,num] = case k of
        'G'              -> Honor (Sangenpai Hatsu)
        'R'              -> Honor (Sangenpai Chun)
        'W' | num == '!' -> Honor (Sangenpai Haku)
            | otherwise  -> Honor (Kazehai Shaa)
        'E'              -> Honor (Kazehai Ton)
        'N'              -> Honor (Kazehai Pei)
        'S' | num == ' ' -> Honor (Kazehai Nan)
            | otherwise  -> Suited SouTile (fromString [num]) False
        'M'              -> Suited ManTile (fromString [num]) False
        'P'              -> Suited PinTile (fromString [num]) False
        _                -> error "no read"
    fromString _       = error "no read"

instance Pretty Tile where
    pretty t = case t of
        Suited ManTile n aka -> (if aka then "m" else "M") <> pretty n
        Suited PinTile n aka -> (if aka then "p" else "P") <> pretty n
        Suited SouTile n aka -> (if aka then "s" else "S") <> pretty n
        Suited{} -> error "No such tile"
        Honor (Sangenpai sang) -> case sang of
                         Haku    -> "W!"
                         Hatsu   -> "G!"
                         Chun    -> "R!"
        Honor (Kazehai kaz) -> case kaz of
                         Ton     -> "E "
                         Nan     -> "S "
                         Shaa    -> "W "
                         Pei     -> "N "
instance Show Tile where showsPrec _ = displayS . renderCompact . pretty
instance Read Tile where
    readsPrec _ = \s -> case lex s of
        ([t],'!':s') : [] -> [(fromString [t,'!'], s')]
        (t,s') : [] -> [(fromString t, s')]
        _ -> []

-- Number
instance Pretty Number where pretty = string . show . (+ 1) . fromEnum
instance IsString Number where
    fromString = toEnum . (\i -> i - 1 :: Int) .
        fromMaybe (error "Number: no read") . readMay

-- Kaze
instance Pretty Kaze where pretty = string . show

-- * Build

suited :: Number -> TileKind -> Tile
suited n tk = Suited tk n False

kaze :: Kaze -> Tile
kaze = Honor . Kazehai

sangen :: Sangen -> Tile
sangen = Honor . Sangenpai

riichiTiles :: [Tile]
riichiTiles = addAka . join . replicate 4 $
    [ Suited k n False | n <- [Ii .. Chuu], k <- [ManTile, PinTile, SouTile] ]
    ++ map (Honor . Sangenpai) [Haku .. Chun]
    ++ map (Honor . Kazehai)   [Ton .. Pei]
  where
    addAka tiles = L.deleteFirstsBy (==~) tiles [ Suited k Wu False | k <- [ManTile, PinTile, PinTile, SouTile] ]
                   & (++ [ Suited k Wu True | k <- [ManTile, PinTile, PinTile, SouTile] ])

-- * Functions

-- | Extract tile kind
tileKind :: Tile -> TileKind
tileKind (Suited k _ _) = k
tileKind (Honor _)      = HonorTile

-- | Number of suited tiles
tileNumber :: Tile -> Maybe Number
tileNumber (Suited _ n _) = Just n
tileNumber (Honor _)      = Nothing

-- | True for Man, Pin and Sou tiles; false for honors.
isSuited :: Tile -> Bool
isSuited = (/= HonorTile) . tileKind

isKaze :: Tile -> Bool
isKaze (Honor (Kazehai _)) = True
isKaze _ = False

suitedSame :: Tile -> Tile -> Bool
suitedSame (Suited tk _ _) (Suited tk' _ _) = tk == tk'
suitedSame _ _ = False

honor :: Tile -> Bool
honor (Honor _) = True
honor _ = False

sangenpai :: Tile -> Bool
sangenpai (Honor (Sangenpai _)) = True
sangenpai _ = False

kazehai :: Tile -> Bool
kazehai (Honor (Kazehai _)) = True
kazehai _ = False

terminal :: Tile -> Bool
terminal t = case tileNumber t of
    Just n -> n == minBound || n == maxBound
    _      -> False

-- * Tile order

-- | @CircularOrd@ models the succession of tiles, dragons and winds.
class CircularOrd a where
    (==~) :: CircularOrd a => a -> a -> Bool
    default (==~) :: Eq a => a -> a -> Bool
    (==~) = (==)
    succCirc :: CircularOrd a => a -> a
    default succCirc :: (Eq a, Bounded a, Enum a) => a -> a
    succCirc x = if x == maxBound then minBound else succ x

instance CircularOrd Number
instance CircularOrd Kaze
instance CircularOrd Sangen

-- | The next tile according to the succession rules. Discards flags like
-- dora from the tile.
instance CircularOrd Tile where
    Suited tk n _ ==~ Suited tk' n' _ = tk == tk' && n == n'
    Honor x       ==~ Honor y         = x == y
    _             ==~ _               = False

    succCirc (Suited k n _) = Suited k (succCirc n) False
    succCirc (Honor (Kazehai k)) = Honor $ Kazehai $ succCirc k
    succCirc (Honor (Sangenpai s)) = Honor $ Sangenpai $ succCirc s

-- | Next or previous kaze. Wraps around.
nextKaze, prevKaze :: Kaze -> Kaze
nextKaze = toEnum . (`mod` 4) . (+ 1) . fromEnum
{-# DEPRECATED nextKaze "use succCirc" #-}
prevKaze = toEnum . (`mod` 4) . (\i -> i - 1) . fromEnum

-- | Like @succ@ and @pred@, but fail as nothing if the succession wouldn't make sense
-- (i.e the input or output would not be a (defined) suited tile).
succMay, predMay :: Tile -> Maybe Tile
succMay (Suited k n a) = Suited k (succ n) a <$ guard (n /= maxBound)
succMay _              = Nothing

predMay (Suited k n a) = Suited k (pred n) a <$ guard (n /= minBound)
predMay _              = Nothing
