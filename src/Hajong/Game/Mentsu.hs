------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Mentsu
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.Mentsu where

import qualified Data.List as L

import Hajong.Game.Tiles

data Mentsu = Shuntsu { mentsuPai :: [Tile], mentsuFrom :: Maybe Shout } -- straight
            | Koutsu  { mentsuPai :: [Tile], mentsuFrom :: Maybe Shout } -- triplet
            | Kantsu  { mentsuPai :: [Tile], mentsuFrom :: Maybe Shout } -- quadret
            | Jantou  { mentsuPai :: [Tile], mentsuFrom :: Maybe Shout } -- pair
            deriving (Show, Read, Eq, Ord) -- TODO Why the ord instance?

data Shout = Pon { shoutedFrom :: Player, shoutedTile :: Tile }
           | Kan { shoutedFrom :: Player, shoutedTile :: Tile }
           | Chi { shoutedFrom :: Player, shoutedTile :: Tile, shoutedTo :: [Tile] }
           | Ron { shoutedFrom :: Player, shoutedTile :: Tile, shoutedTo :: [Tile] }
           deriving (Show, Read, Eq, Ord) -- TODO Check the ord instance

-- * Build

koutsu :: [Tile] -> Mentsu
koutsu = flip Koutsu Nothing

kantsu :: [Tile] -> Mentsu
kantsu = flip Kantsu Nothing

jantou :: [Tile] -> Mentsu
jantou = flip Jantou Nothing

shuntsu :: [Tile] -> Mentsu
shuntsu = flip Shuntsu Nothing

fromShout :: Shout -> Mentsu
fromShout shout
    | Pon{} <- shout           = Koutsu (replicate 3 $ shoutedTile shout) (Just shout)
    | Kan{} <- shout           = Kantsu (replicate 4 $ shoutedTile shout) (Just shout)
    | Chi{} <- shout           = Shuntsu (shoutedTile shout : shoutedTo shout) (Just shout)
    | Ron{} <- shout
    , [_]   <- shoutedTo shout = Jantou (replicate 2 $ shoutedTile shout) (Just shout)
    | [x,y] <- shoutedTo shout
    , x == y                  = Kantsu (replicate 3 $ shoutedTile shout) (Just shout)
    | otherwise               = Shuntsu (shoutedTile shout : shoutedTo shout) (Just shout)

-- * Check

type Mentsus = [Mentsu]
type CompleteHand = [Mentsu]

handComplete :: Mentsus -> [Tile] -> Bool
handComplete mentsu = not . null .
    mapMaybe (isComplete . (mentsu ++)) . mentsuSearch

mentsuSearch :: [Tile] -> [Mentsus]
mentsuSearch =
    buildVariations . map getMentsu .
    groupBy compareSuit . sort

buildVariations :: [[Mentsus]] -> [Mentsus]
buildVariations = go
    where go (x : xs) = x >>= (`map` go xs) . (++)
          go []       = []

-- | This gets only total mentsu: no orphan tiles are left in the result.
getMentsu :: [Tile] -> [Mentsus]
getMentsu tiles = map fst $ go ([], tiles)
    where 
        go :: ([Mentsu], [Tile]) -> [([Mentsu], [Tile])] -- (mentsu, leftovers)
        go (done, xs@(a:b:es)) = let

            takingJantou  = if a == b then go (Jantou [a,b] Nothing : done, es) ++ takingKoutsu else []
            takingKoutsu  = case es of
                (c:es') | c == a      -> go (Koutsu [a,b,c] Nothing : done, es') ++ takingKantsu
                        | otherwise   -> []
                _                     -> []
            takingKantsu  = case es of
                (c:d:es') | c == d    -> go (Kantsu [a,b,c,d] Nothing : done, es')
                          | otherwise -> []
                _                     -> []
            takingShuntsu = case shuntsuOf a xs of
                Just ment   -> go (ment : done, xs L.\\ mentsuPai ment)
                Nothing     -> []
            in takingJantou ++ takingShuntsu

        go (done, []) = [(done, [])]
        go (_,  _)    = [] -- branch cannot be complete with one orphan tile

shuntsuOf :: Tile -> [Tile] -> Maybe Mentsu
shuntsuOf a xs = do
    r <- tileSucc a
    s <- tileSucc r
    guard (tileSuited a && r `elem` xs && s `elem` xs)
    return $ Shuntsu [a,r,s] Nothing

isComplete :: Mentsus -> Maybe CompleteHand
isComplete xs = do
    guard $ length xs == 5
    guard $ length (filter isJantou xs) == 1
    return xs


-- ** Checks

isShuntsu' :: [Tile] -> Bool
isShuntsu' xs = case sort xs of
    (x:y:z:[]) -> tileSuited x && Just y == tileSucc x && Just z == tileSucc y
    _ -> False

-- | Documentation for 'isJantou'
isJantou, isShuntsu, isKoutsu, isKantsu :: Mentsu -> Bool
isJantou x
    | Jantou{} <- x = True
    | otherwise    = False
isShuntsu x
    | Shuntsu{} <- x = True
    | otherwise     = False
isKoutsu x
    | Koutsu{} <- x = True
    | otherwise    = False
isKantsu x
    | Kantsu{} <- x = True
    | otherwise    = False
