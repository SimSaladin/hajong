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
import Text.PrettyPrint.ANSI.Leijen ((<>), displayS, renderCompact, dquotes)
import qualified Data.List as L
------------------------------------------------------------------------------

-- * Types

-- | A (japanese) mahjong tile. Note the lack of Eq/Ord, because tiles can
-- have flags.
data Tile = Suited TileKind Number Aka
          | Honor Honor

-- | A wrapped tile, with Eq and Ord via (==~), e.g. semantic equality.
-- Discards flags like aka-dora info.
newtype TileEq = TileEq Tile deriving (Show, Read, CircularOrd)

instance Eq TileEq where a == b = a ==~ b
instance Ord TileEq where
    TileEq (Honor h) <= TileEq (Honor h')              = h <= h'
    TileEq (Suited tk n _) <= TileEq (Suited tk' n' _) = (tk, n) <= (tk', n')
    TileEq Suited{} <= TileEq Honor{}                  = True
    _ <= _                                             = False

-- | XXX: The derived, "real" Eq instance would be useful for Tile in e.g.
-- when working with calls and mentsu. But then the Algo-module would
-- break. Should perhaps refactor things where the semantic equality is
-- important to use the @TileEq@ wrapper, and derive the Tile instances.
instance Eq Tile where (==) = (==~)
instance Ord Tile where
    Honor h <= Honor h'              = h <= h'
    Suited tk n _ <= Suited tk' n' _ = (tk, n) <= (tk', n')
    Suited{} <= Honor{}              = True
    _ <= _                           = False

-- | Exact equality on tiles
(==@) :: Tile -> Tile -> Bool
a ==@ b = a ==~ b && equating isAka a b

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
        's'              -> Suited SouTile (fromString [num]) True
        'm'              -> Suited ManTile (fromString [num]) True
        'p'              -> Suited PinTile (fromString [num]) True
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
instance Show Tile where showsPrec _ = displayS . renderCompact . dquotes . pretty
instance Read Tile where
    readsPrec _ = \s -> case lex s of
        ('"':xs, str) : []  -> [(fromString (initEx xs), str)]
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

isAka :: Tile -> Bool
isAka (Suited _ _ True) = True
isAka _ = False

-- | Sets a suited tile as aka. id on honors.
setAka :: Tile -> Tile
setAka (Suited s n _) = Suited s n True
setAka x = x

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

removeFlags :: Tile -> Tile
removeFlags (Suited tk n _) = Suited tk n False
removeFlags x               = x

-- * Tile order

-- | @CircularOrd@ models the succession of tiles, dragons and winds.
class CircularOrd a where
    (==~) :: CircularOrd a => a -> a -> Bool
    succCirc, predCirc :: CircularOrd a => a -> a
    succMay, predMay :: CircularOrd a => a -> Maybe a

    default (==~) :: Eq a => a -> a -> Bool
    default succCirc :: (Eq a, Bounded a, Enum a) => a -> a
    default predCirc :: (Eq a, Bounded a, Enum a) => a -> a
    default succMay :: (Eq a, Bounded a, Enum a) => a -> Maybe a
    default predMay :: (Eq a, Bounded a, Enum a) => a -> Maybe a
    (==~)      = (==)
    succCirc x = if x == maxBound then minBound else succ x
    predCirc x = if x == minBound then maxBound else pred x
    succMay x  = if x == maxBound then Nothing else Just (succ x)
    predMay x  = if x == minBound then Nothing else Just (pred x)

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

    predCirc (Suited k n _) = Suited k (predCirc n) False
    predCirc (Honor (Kazehai k)) = Honor $ Kazehai $ predCirc k
    predCirc (Honor (Sangenpai s)) = Honor $ Sangenpai $ predCirc s

    succMay (Suited k n _) = Suited k (succ n) False <$ guard (n /= maxBound)
    succMay _              = Nothing -- TODO implement succession of honors here

    predMay (Suited k n _) = Suited k (pred n) False <$ guard (n /= minBound)
    predMay _              = Nothing
