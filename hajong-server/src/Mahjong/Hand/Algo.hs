{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Hand.Algo
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Algorithms to work with mahjong hands.
------------------------------------------------------------------------------
module Mahjong.Hand.Algo
    (
    -- * Shanten
    Shanten,
    HasGroupings(..),
    shanten, shantenBy, tenpai, complete,
    chiitoitsuShanten, kokushiShanten,

    -- * Tile grouping
    tilesGroupL, tilesSplitGroupL,
    leftovers, waits, tileGroupTiles,

    -- * Types
    Wait, Grouping, TileGroup(..),
    ) where

------------------------------------------------------------------------------
import           Import
import           Mahjong.Hand.Internal
import           Mahjong.Hand.Mentsu
import           Mahjong.Tiles

------------------------------------------------------------------------------
import           Data.Maybe (fromJust)
import           Data.List (delete, nub)
import qualified Data.List as L
import qualified Text.PrettyPrint.ANSI.Leijen as P

------------------------------------------------------------------------------

-- Types

-- | Right for shuntsu wait, Left for koutsu wait or ready pair
type Wait = Either Tile [Tile]

-- | A nothing result means that the hand has 14 (or more) tiles but is not
-- complete. Thus, invalid.
type Shanten = Maybe Int

-- | A single grouping variant.
type Grouping = [TileGroup]

-- | Data type used to describe some group of tiles.
data TileGroup = GroupWait MentsuKind [Tile] [Tile]
                -- ^ A @MentsuWait kind Inhand waits@ describes a wait on
                -- any of the tiles `waits` for a mentsu of kind `kind`
                -- with tiles `inhand` already in hand.
                --
                -- Note that Jantou == GroupWait Koutsu [t,t] [t]
                | GroupComplete Mentsu
                -- ^ Note that jantou (the pair) are viewed as koutsu
                -- waits. It's pretty logical when you think about it.
                | GroupLeftover Tile
                -- ^ A leftover tile which cannot be associated with any
                -- other tile.
                deriving (Show, Read, Eq, Ord)

instance P.Pretty TileGroup where
    pretty (GroupWait _ inh _) = "w" P.<> P.tupled (map P.pretty inh)
    pretty (GroupComplete m)   = "c" P.<> P.tupled (map P.pretty $ mentsuTiles m)
    pretty (GroupLeftover t)   = "l" P.<> P.tupled [P.pretty t]

-- Tiles split

-- | `tilesGroupL` simply returns all possible groupings for the tiles
--
-- The algorithm goes through the input list of tiles (after sorting it)
-- trying to use the first tile (head of the list) to form:
--
--  1. an ankan
--  2. an ankou
--  3. an ankou wait
--  4. a shuntsu
--  5. a shuntsu wait with its successor
--  6. a shuntsu wait the successor's successor (middle wait), or
--  7. discard the tile as a `GroupLeftOver`.
--
-- Whenever any of these are possible, the result is cons'ed to every
-- result from applying the function recurively on the remaining tiles.
-- The end result is a list of all possible groups.
--
-- = Performance concerns and optimizations
--
-- Complete mentsu are always preferred over incomplete (and incomplete
-- over leftovers) and are placed at the head of the list /on that level of
-- recursion/.  However, there is no guarentee that minimal groupings
-- couldn't reside in the result's tail.
--
-- There are a few cases when the same grouping is considered twice or
-- more. For instance, 223 is taken as [22, 3], [23, 2], [2, 23] and [2, 2,
-- 3]. Notice the two duplicates. Same thing for 222: [222], [22, 2], [2,
-- 22] and [2,2,2].  A fix for this would be rather tricky to add, but
-- maybe if we changed the input from [Tile] to [(Tile, Int)] where second
-- value stands for the number of those tiles it could fit well.
tilesGroupL :: [Tile] -> [Grouping]
tilesGroupL = go . L.sort
    where
        go []       = [ [] ]
        go [x]      = [ [GroupLeftover x] ]
        go (x:y:xs) = takeKantsu ++ takeTriplet ++ takePair ++ takeShuntsu ++ dropOne
            where
                takePair
                    | x == y                            = GroupWait Koutsu [x, x] [x] `goWith` xs
                    | otherwise                         = []

                takeTriplet
                    | (z:xs') <- xs, x == z              = GroupComplete (koutsu x) `goWith` xs'
                    | otherwise                         = []

                takeKantsu
                    | (z:w:xs') <- xs, x == z, z == w    = GroupComplete (kantsu x) `goWith` xs'
                    | otherwise                         = []

                takeShuntsu = concat $ catMaybes
                        [ ts_complete    <$> my <*> mz
                        , ts_sequentical <$> my
                        , ts_inbetween   <$> mz
                        ]

                my = succMay x >>= \y' -> y' <$ guard (y' `elem` (y:xs))
                mz = succMay x >>= succMay >>= \z -> z <$ guard (z `elem` (y:xs))

                ts_complete y' z  = GroupComplete (shuntsu x) `goWith` delete y' (delete z (y:xs))
                ts_sequentical y' = GroupWait Shuntsu [x,y'] (catMaybes [predMay x, succMay y']) `goWith` delete y' (y:xs)
                ts_inbetween   z  = GroupWait Shuntsu [x,z] [fromJust (succMay x)] `goWith` delete z (y:xs)

                dropOne = GroupLeftover x `goWith` (y:xs)

                goWith a = map (a :) . go

-- | A follow-up optimization on `tilesGroupL` that first groups the input
-- tiles by their suit or honor kind, then feeds the groups separetely
-- to `tilesGroupL` and finally joins the results.
tilesSplitGroupL :: [Tile] -> [Grouping]
tilesSplitGroupL = combine . map tilesGroupL . groupBy compareKind . sort
    where
        a `compareKind` b = tileKind a == tileKind b

        -- | outer list : tile kinds
        --   middle list : tilegroup combinations
        --   inner list : tilegroups for the combination all of the same tile kind
        combine :: [[[a]]] -> [[a]]
        combine       [] = [[]]
        combine (tk:tks) = [ xs ++ ys | xs <- tk, ys <- combine tks ]

-- Shanten

-- | We use a type class to allow calculating shanten (tiles away from
-- tenpai) from different representations.
--
-- Note that instances strictly assume valid input: 13 or 14 tile hands, or
-- including kan in fst in the ([Mentsu], [Tile]) instance.
--
-- The return value is /strictly positive/ for shanten, /exactly zero/ for
-- tenpai and /negative/ (-1) for complete hands.
--
-- /Technical notes./ Even though the subtract algorithm gives -1 for all
-- complete hands, not all -1 indicate a complete hand. Consider for
-- example a hand with 4x complete melds and tiles of a shuntsu wait: the
-- algorithm thinks this is a complete hand when in fact the pair is
-- missing. Therefore we check the condition that the pair is there and
-- then return @(Just -1)@. In the case of an invalid complete hand we
-- return `Nothing`.
class HasGroupings x where
    getGroupings :: x -> [Grouping]

instance HasGroupings Grouping where getGroupings = return
instance HasGroupings [Tile]   where getGroupings = tilesSplitGroupL

instance HasGroupings ([Mentsu], [Tile]) where
    getGroupings (ms, ts) = map (map GroupComplete ms ++) (tilesSplitGroupL ts)

instance HasGroupings [x] => HasGroupings [[x]] where
    getGroupings xs = let gs     = map getGroupings xs
                          ss     = map (minimumEx . map shanten') gs
                          min_ss = minimumEx ss
                          in concatMap fst . filter ((== min_ss) . snd) $ zip gs ss

instance HasGroupings HandA where
    getGroupings h = getGroupings $ (,)
        <$> _handCalled
        <*> (liftA2 (++) (map pickedTile._handPicks) (runIdentity._handConcealed)) $ h

shanten :: HasGroupings x => x -> Shanten
shanten = shantenBy shanten'

shantenBy :: HasGroupings x => (Grouping -> Shanten) -> x -> Shanten
shantenBy f = minimumMay . mapMaybe f . getGroupings

complete, tenpai :: HasGroupings x => x -> Bool
complete = (== Just (-1)) . shanten
tenpai   = (== Just 0) . shanten

shanten' :: Grouping -> Shanten
shanten' = fmap minimumEx . sequence <$> sequence
    [ groupingShanten 8
    , chiitoitsuShanten
    , kokushiShanten
    ]

-- | @groupingShanten n tgs@ calculates shanten of `tgs` using the subtract
-- algorithm starting at `n`, with additional check for complete hand
-- support (invalid (Nothing) if no pair).
--
-- = Technical details
--
-- The subtract algorithm substracts from 8 2 for every complete mentsu,
-- 1 for pairs of related tiles and adds (max 0, number of shuntsu or
-- shuntsu waits - 4).
--
-- The last addition comes from the fact that with 5 complete mentsu or
-- shuntsu waits one of them /has/ to be discarded before tenpai (because
-- there can only be a maximum of 4 complete mentsu in a hand), thus there
-- has to be a tile switch which won't change shanten. In the case of
-- 6 such groups there has to be two switches not affecting shanten (thus
-- +2).
--
-- 7 and more are impossible cases (2*7 = 14 > 13) except when a complete
-- hand is encountered; in the case of 4 shuntsu and one shuntsu wait
-- a Nothing is returned to indicate invalid or non-winning hand.
groupingShanten :: Int -> Grouping -> Shanten
groupingShanten n tgs = case foldl' (\i -> (i -) . tgval) n tgs of
    -1 | length (filter isPair tgs) /= 1 -> Nothing
    s -> Just $ s + max 0 (length (filter notPairable tgs) - 4)
  where
    tgval (GroupComplete m)
        | Jantou <- mentsuKind m = 1
        | otherwise              = 2
    tgval (GroupWait{})          = 1
    tgval (GroupLeftover{})      = 0

kokushiShanten, chiitoitsuShanten :: Grouping -> Shanten
kokushiShanten = Just . (13 -) . length . nub. filter suitedOrTerminal . concatMap tileGroupTiles
  where suitedOrTerminal = liftA2 (||) honor terminal
chiitoitsuShanten = Just . (6 -) . length . filter isPair

-- Auxilary funtions

isPair, notPairable :: TileGroup -> Bool
isPair (GroupWait Koutsu _ _)              = True
isPair (GroupComplete (Mentsu Jantou _ _)) = True
isPair                      _              = False

notPairable (GroupComplete  (Mentsu Jantou _ _)) = False
notPairable (GroupComplete _)                    = True
notPairable (GroupWait Shuntsu _ _)              = True
notPairable                       _              = False

-- | Leftover tiles in the hand.
leftovers :: Grouping -> [Tile]
leftovers = mapMaybe go
    where
        go (GroupLeftover x) = Just x
        go _                 = Nothing

-- | Waits in the hand. Inner lists represent choice.
waits :: Grouping -> [Wait]
waits = mapMaybe go
    where
        go (GroupWait Koutsu  _ [t]) = Just $ Left t
        go (GroupWait Shuntsu _ xs)  = Just $ Right xs
        go _                         = Nothing

-- | Get the tiles in hand from a @TileGroup@.
tileGroupTiles :: TileGroup -> [Tile]
tileGroupTiles (GroupWait _ tiles _)  = tiles
tileGroupTiles (GroupComplete mentsu) = mentsuTiles mentsu
tileGroupTiles (GroupLeftover tile)   = [tile]
