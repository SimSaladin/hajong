------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Tiles
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.Tiles where

import ClassyPrelude
import Control.Applicative

newtype Player = Player Kazehai deriving (Show, Read, Eq, Ord)
deriving instance Enum Player

data Number = Ii | Ryan | San | Suu | Wu | Rou | Chii | Paa | Chuu
            deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Sangenpai = Haku | Hatsu | Chun
               deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Kazehai = Ton | Nan | Shaa | Pei
             deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Tile = Man Number Bool
          | Pin Number Bool
          | Sou Number Bool
          | Sangen Sangenpai
          | Kaze Kazehai
          deriving (Show, Read, Eq, Ord)

data Mentsu = Shuntsu { mentsuPai :: [Tile], mentsuOpen :: Maybe Player }  -- straight
            | Koutsu  { mentsuPai :: [Tile], mentsuOpen :: Maybe Player } -- triplet
            | Kantsu  { mentsuPai :: [Tile], mentsuOpen :: Maybe Player } -- quadret
            | Jantou  { mentsuPai :: [Tile], mentsuOpen :: Maybe Player } -- pair
            deriving (Show, Read, Eq, Ord)

data Shout = Pon
           | Kan
           | Chi (Tile, Tile)
           | Ron
           deriving (Show, Read, Eq)

koutsu :: [Tile] -> Mentsu
koutsu = flip Koutsu Nothing

kantsu :: [Tile] -> Mentsu
kantsu = flip Kantsu Nothing

jantou :: [Tile] -> Mentsu
jantou = flip Jantou Nothing

shuntsu :: [Tile] -> Mentsu
shuntsu = flip Shuntsu Nothing

riichiTiles :: [Tile]
riichiTiles = join . replicate 4 $ 
    concatMap (\suit -> map (`suit` False) [Ii .. Chuu]) [Man, Pin, Sou]
    ++ map Sangen [Haku .. Chun]
    ++ map Kaze [Ton .. Pei]

-- | True for suited tiles only
tileSuited :: Tile -> Bool
tileSuited (Man _ _) = True
tileSuited (Pin _ _) = True
tileSuited (Sou _ _) = True
tileSuited _ = False

tileNumber :: Tile -> Number
tileNumber (Man n _) = n
tileNumber (Pin n _) = n
tileNumber (Sou n _) = n
tileNumber _         = error "Not suited tile"

tileSangenpai :: Tile -> Bool
tileSangenpai (Sangen _) = True
tileSangenpai _          = False

setTileNumber :: Tile -> Number -> Tile
setTileNumber (Man _ r) n = Man n r
setTileNumber (Pin _ r) n = Pin n r
setTileNumber (Sou _ r) n = Sou n r
setTileNumber _         _ = error "Not suited tile"

compareSuit :: Tile -> Tile -> Bool
compareSuit (Man _ _) (Man _ _)   = True
compareSuit (Pin _ _) (Pin _ _)   = True
compareSuit (Sou _ _) (Sou _ _)   = True
compareSuit (Sangen _) (Sangen _) = True
compareSuit (Kaze _) (Kaze _)     = True
compareSuit _ _                   = False

tileTerminal :: Tile -> Bool
tileTerminal tile = tileSuited tile && liftA2 (||) (== Ii) (== Chuu) (tileNumber tile)

tileSucc :: Tile -> Maybe Tile
tileSucc (Kaze kaze)
                | kaze == maxBound           = Nothing
                | otherwise                  = Just $ Kaze (succ kaze)
tileSucc (Sangen sangen)
               | sangen == maxBound          = Nothing
               | otherwise                   = Just $ Sangen (succ sangen)
tileSucc tile  | tileNumber tile == maxBound = Nothing
               | otherwise                   = Just $ setTileNumber tile $ succ $ tileNumber tile
