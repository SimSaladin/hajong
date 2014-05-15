module Tiles where

import ClassyPrelude


data Number = Ii | Ryan | San | Suu | Wu | Rou | Chii | Paa | Chuu
            deriving (Show, Read, Eq, Ord, Enum)

data Sangenpai = Haku | Hatsu | Chun
               deriving (Show, Read, Eq, Enum)

data Kazehai = Ton | Nan | Shaa | Pei
             deriving (Show, Read, Eq, Enum)

data Tile = Man Number Bool
          | Pin Number Bool
          | Sou Number Bool
          | Sangen Sangenpai
          | Kaze Kazehai
          deriving (Show, Read, Eq)

data Mentsu = Shuntsu { mentsuPai :: [Tile], mentsuOpen :: Bool }  -- straight
            | Koutsu  { mentsuPai :: [Tile], mentsuOpen :: Bool } -- triplet
            | Kantsu  { mentsuPai :: [Tile], mentsuOpen :: Bool } -- quadret
            | Jantou  { mentsuPai :: [Tile], mentsuOpen :: Bool } -- pair
            deriving (Show, Read, Eq)

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

setTileNumber :: Tile -> Number -> Tile
setTileNumber (Man _ r) n = Man n r
setTileNumber (Pin _ r) n = Pin n r
setTileNumber (Sou _ r) n = Sou n r
setTileNumber _         n = error "Not suited tile"
