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
    chiitoitsuShanten, kokushiShanten, kokushiAgari, groupingShanten, tenpaiGroupings,

    -- * Tile grouping
    tilesGroupL, tilesSplitGroupL,
    leftovers, waits, tileGroupTiles, isPair,
    getAgari,

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
import           Data.List (delete)
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
                deriving (Show, Read, Ord, Eq)

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
tilesGroupL = go . L.sortOn TileEq
    where
        go []       = [ [] ]
        go [x]      = [ [GroupLeftover x] ]
        go (x:y:xs) = takeKantsu ++ takeTriplet ++ takePair ++ takeShuntsu ++ dropOne
            where
                takePair
                    | x ==~ y                           = GroupWait Koutsu [x, x] [x] `goWith` xs
                    | otherwise                         = []

                takeTriplet
                    | (z:xs') <- xs, x ==~ z             = GroupComplete (koutsu x) `goWith` xs'
                    | otherwise                         = []

                takeKantsu
                    | (z:w:xs') <- xs, x ==~ z, z ==~ w = GroupComplete (kantsu x) `goWith` xs'
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

instance HasGroupings Hand where
    getGroupings hand = getGroupings $ (\mentsu concealed magari -> case magari of
                                       Nothing    -> (mentsu, concealed)
                                       Just (AgariTsumo tile _) -> (mentsu, tile : concealed)
                                       Just (AgariCall shout) | length (shoutTo shout) < 2 -> (mentsu, shoutTile shout : shoutTo shout ++ concealed)
                                                              | otherwise            -> (fromShout shout : mentsu, concealed) )
                                       -- XXX: special shouts, where shoutTo /= [a,b], are regarded as individual tiles.
        <$> _handCalled
        <*> liftA2 (++) (map pickedTile._handPicks) _handConcealed
        <*> _handAgari
        $ hand

shanten :: HasGroupings x => x -> Shanten
shanten = shantenBy shanten'

shantenBy :: HasGroupings x => (Grouping -> Shanten) -> x -> Shanten
shantenBy f = minimumMay . mapMaybe f . getGroupings

complete, tenpai :: HasGroupings x => x -> Bool
complete = (== Just (-1)) . shanten
tenpai   = (== Just 0) . shanten

tenpaiGroupings :: HasGroupings x => x -> [Grouping]
tenpaiGroupings = filter tenpai . getGroupings

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
    s  | 0 <- s + pairExtra, [t] <- getAgari tgs, length (filter (== t) $ concatMap tileGroupTiles tgs) == 4 -> Nothing -- XXX: No shanten implied if waiting only on a tile we already have 4 of. correct would be to build some other wait - how many would that take?
       | 0 <- s + pairExtra, [] <- getAgari tgs -> Nothing -- XXX: Also, if there simply isn't a tile in the world which could complete the hand, it's not tenpai
       | otherwise -> Just (s + pairExtra)
  where
    tgval (GroupComplete m)
        | Jantou <- mentsuKind m = 1
        | otherwise              = 2
    tgval (GroupWait{})          = 1
    tgval (GroupLeftover{})      = 0
    pairExtra = max 0 (length (filter notPairable tgs) - 4)

-- | If there is a shuntsu wait, that is the only possible agari. If there
-- is a single leftover tile that is the agari. otherwise any of the koutsu
-- waits can be completed (thus agari).
getAgari :: Grouping -> [Tile]
getAgari xs | [GroupWait Shuntsu _ ws] <- filter isShuntsuWait xs  = ws -- shuntsu wait
            | [t] <- leftovers xs                                  = [t] -- tanki wait
            | Just x <- kokushiAgari (concatMap tileGroupTiles xs) = either return id x -- kokushi wait
            | [GroupLeftover t] <- filter (not . isPair) xs        = [t] -- chiitoi ("tanki") wait
            | otherwise                                            = L.nub $ concatMap (either return id) $ waits xs -- koutsu wait; XXX: nub'ed because think double koutsu wait: [x, x, x, x] results in [x,x] waits

isShuntsuWait :: TileGroup -> Bool
isShuntsuWait (GroupWait Shuntsu _ _) = True
isShuntsuWait _                       = False

-- * Non-standard assembly

-- | TODO: This doesn't require groupings. would be more efficient to check
-- once with all the tiles.
chiitoitsuShanten :: Grouping -> Shanten
chiitoitsuShanten = Just . (6 -) . length . filter isPair

-- | TODO: This doesn't require groupings. would be more efficient to check
-- once with all the tiles.
kokushiShanten :: Grouping -> Shanten
kokushiShanten grp = case concatMap tileGroupTiles grp L.\\ ["P1", "P9", "M1", "M9", "S1", "S9", "E", "S", "W", "N", "G", "R", "W!"] of
    []                          -> Just 0
    [x] | honor x || terminal x -> Just (-1)
    _                           -> Just 13
            -- TODO: this branch is hard, because we can have even 18
            -- tiles in the hand at the moment. should discard kokushi
            -- altogether when there are melds.

-- | One of:
--  * no agari (complete or some crap)
--  * any honor or terminal
--  * specific honor or terminal
kokushiAgari :: [Tile] -> Maybe (Either Tile [Tile])
kokushiAgari tiles = case tiles L.\\ terminalsAndHonors of
    []                                              -> Just (Right terminalsAndHonors)
    [x] | length tiles == 13, honor x || terminal x -> Just $ Left $ headEx $ terminalsAndHonors L.\\ tiles
    _                                               -> Nothing
  where terminalsAndHonors = ["P1", "P9", "M1", "M9", "S1", "S9", "E", "S", "W", "N", "G", "R", "W!"]

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
