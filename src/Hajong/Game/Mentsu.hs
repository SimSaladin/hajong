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

import           ClassyPrelude
import qualified Data.List as L

import Hajong.Game.Types
import Hajong.Game.Tiles

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
