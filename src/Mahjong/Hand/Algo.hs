{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Hand.Algo
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
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
    shanten, tenpai, complete,

    {- buildGWTs, buildGWTs', buildGWT, -}
    {- minDepth, levels, -}

    -- * Tile grouping
    tilesGroupL, tilesSplitGroupL,
    leftovers, waits, tileGroupTiles,

    -- * Types
    {- WaitTree, -} Wait, Grouping,
    TileGroup(..),

    -- * Misc
    devops
    ) where

import           Data.Maybe
import           Data.Either (isLeft)
-- import           Data.Bifunctor
import           Data.List (delete, nub)
import qualified Data.List as L
import           Data.List.HT (removeEach)
import qualified Data.List.NonEmpty as NE

-- import Mahjong.Hand.Algo.WaitTree as Mahjong.Hand.Algo
import Mahjong.Hand.Mentsu
import Mahjong.Tiles

-- Types

-- | Right for shuntsu wait, Left for koutsu wait or ready pair
type Wait = Either Tile [Tile]

-- | A nothing result means that the hand has 14 (or more) tiles but is not
-- complete. Thus, invalid.
type Shanten = Maybe Int

-- | Non-recursive helper type for the unfolder below.
--
-- Left for ready with, Right for (discarding, getting, stray tiles, waits).
type DevOp' = Either Tile (Tile, Tile, [Tile], [Wait])

-- | A single grouping variant.
type Grouping = [TileGroup]

-- | Data type used to describe some group of tiles.
data TileGroup = GroupWait MentsuKind [Tile] [Tile]
                -- ^ A @MentsuWait kind Inhand waits@ describes a wait on
                -- any of the tiles `waits` for a mentsu of kind `kind`
                -- with tiles `inhand` already in hand.
                | GroupComplete Mentsu
                -- ^ Note that jantou (the pair) are viewed as koutsu
                -- waits. It's pretty logical when you think about it.
                | GroupLeftover Tile
                -- ^ A leftover tile which cannot be associated with any
                -- other tile.
                deriving (Show, Read, Eq, Ord)

{- 
-- | Development operation that waits for tile `waitFor` discarding
-- `waitDiscard`.
data DevOp = DevOp { opDiscard  :: Tile
                   , opFor      :: Tile
                   } deriving (Eq, Ord, Show)
                   -}

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
instance HasGroupings [Tile] where getGroupings = tilesSplitGroupL
instance HasGroupings ([Mentsu], [Tile]) where getGroupings (ms, ts) = map (map GroupComplete ms ++) (tilesSplitGroupL ts)

shanten :: HasGroupings x => x -> Shanten
shanten = go . mapMaybe shanten' . getGroupings
    where
        go [] = Nothing
        go xs = Just $ minimumEx xs

complete :: HasGroupings x => x -> Bool
complete = (== Just (-1)) . shanten

tenpai :: HasGroupings x => x -> Bool
tenpai = (== Just 0) . shanten

shanten' :: Grouping -> Shanten
shanten' = liftM (fmap minimumEx . sequence) $ sequence
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
                    -1 | Nothing <- find isPair tgs -> Nothing
                        -- complete (-1) only when there are 14 or more
                        -- tiles, so without a pair it's invalid hand
                    s -> Just $ s + max 0 (length (filter notPairable tgs) - 4)
    where
        tgval (GroupWait{})     = 1
        tgval (GroupComplete{}) = 2
        tgval (GroupLeftover{}) = 0

        notPairable (GroupComplete _)       = True
        notPairable (GroupWait Shuntsu _ _) = True
        notPairable                       _ = False

kokushiShanten :: Grouping -> Shanten
kokushiShanten = Just . (13 -) . length . nub . filter ((||) <$> not . suited <*> isTerminal) . concatMap tileGroupTiles

isTerminal :: Tile -> Bool
isTerminal t = Just minBound == tileNumber t || tileNumber t == Just maxBound

chiitoitsuShanten :: Grouping -> Shanten
chiitoitsuShanten = Just . (6 -) . length . filter isPair

isPair :: TileGroup -> Bool
isPair (GroupWait Koutsu _ _) = True
isPair                      _ = False

-- Wait trees

-- | @devops free_discards waits@ returns all greedy development options
-- ("@DevOp@") where a tile from @free_discards@ is discarded to draw
-- some tile to complete a meld in @waits@.
--
-- = Algorithm description
--
-- 1.  if no free and one or less waits: error "malformed hand".
-- 2.  if two waits of which at least one is a koutsu wait (a pair): a tenpai
--     wait.
-- 3.  if no free: break up a wait.
-- 4.  if one free and no waits: pair wait tenpai.
-- 5.  if no waits: leftovers only: build waits from free tiles.
-- 6.  otherwise meld by discarding free to complete waits.
--
devops :: [Tile] -> [Wait] -> NE.NonEmpty DevOp'
devops f_ts w_ts
    | []  <- f_ts, []  <- w_ts     = error "devops: called with a malformed hand (all mentsu)"
    | []  <- f_ts, [_] <- w_ts     = error "devops: called with a malformed hand (one mentsu free only)"
    | []  <- f_ts, tenpaiWaits    = NE.fromList tenpaiHasKoutsu
    | []  <- f_ts                 = NE.fromList breakingWait
    | [x] <- f_ts, []  <- w_ts     = return (Left x) -- Last pair wait
    |             []  <- w_ts     = NE.fromList leftoversOnly
    | otherwise                  = NE.fromList meldLeftovers
    where
        tenpaiWaits = length w_ts == 2 && any isLeft w_ts

        tenpaiHasKoutsu
            | [Left x, Left y] <- w_ts = [Left x, Left y]
            | otherwise               = concatMap (either (const []) (map Left)) w_ts

        meldLeftovers = melding f_ts w_ts ++ meldingFree

        -- discard a free tile to "meld" to another free tile a wait.
        meldingFree = do
            (f_t, f_ts') <- removeEach f_ts
            (wait_target, f_ts'') <- removeEach f_ts'
            (draw, new_wait) <- buildWaits wait_target
            return $ Right (f_t, draw, f_ts'', new_wait : w_ts)

        breakingWait = do
            (w_b, w_ts') <- removeEach w_ts
            let f_ts' = either (\t -> [t,t]) shuntsuWaitToHandTiles w_b
                in melding f_ts' w_ts'

        leftoversOnly = do
            (w_t, f_ts')  <- removeEach f_ts
            (d_t, f_ts'') <- removeEach f_ts'
            (draw, wait)  <- buildWaits w_t
            return $ Right (d_t, draw, f_ts'', [wait])

        -- All combinations of discarding from f_all and melding to one of
        -- w_all
        melding f_all w_all = do
            (d_t, f_ts') <- removeEach f_all
            (w_opts, w_ts') <- removeEach w_all
            draw  <- either return id w_opts
            return $ Right (d_t, draw, f_ts', w_ts')

{-

-- | In a greedy wait tree every @DevOp@ brings the hand closer to
-- tenpai/win.
--
-- The tree is formed by splitting the tiles with @tilesSplitGroupL@ and
-- iterating different cases of replacing @GroupLeftover@ with @DevOp@ that
-- would complete some @GroupWait@ to @GroupComplete@, or another
-- @GroupLeftover@ to a @GroupWait@.
--
-- NOTE: This does /not/ check tile count changes if kantsu are found in
-- input tiles - tree building /will/ fail if there are uncalled kantsu in
-- the input tiles!
buildGWTs' :: [Mentsu] -> [Tile] -> [WaitTree]
buildGWTs' ms ts = buildGWTs (map (map GroupComplete ms ++) $ tilesSplitGroupL ts)

-- | Build wait trees of all groupings for which shanten == min_shanten.
buildGWTs :: [Grouping] -> [WaitTree]
buildGWTs gs = map buildGWT $ filter ((== min_s) . shanten) gs
    where
        min_s = shanten gs

-- | @buildGWT group@ discards groupings strictly less than shanten over
-- `groups` before building the tree.
buildGWT :: Grouping -> WaitTree
buildGWT g = unfoldRootedTree g go (NE.toList $ devops <$> leftovers <*> waits $ g)
    where
        go :: DevOp' -> Either TenpaiOp (DevOp, NE.NonEmpty DevOp')
        go = bimap (`TenpaiOp` 0)
                   (\(disc, draw, lo, wa) -> (DevOp disc draw, devops lo wa))

-}

-- Auxilary funtions

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

-- | Get the tiles in hand of the shuntsu wait based on the tiles waited.
shuntsuWaitToHandTiles :: [Tile] -> [Tile]
shuntsuWaitToHandTiles [] = error "shuntsuWaitToHandTiles: empty list of waits"
shuntsuWaitToHandTiles [t] = catMaybes $ case tileNumber t of
    Just San -> [predMay t, predMay t >>= predMay]
    Just Chii -> [succMay t, succMay t >>= succMay]
    _ -> [predMay t, succMay t]
shuntsuWaitToHandTiles (t:_) = catMaybes [succMay t, succMay t >>= succMay]

-- | Build possible waits for the tile, always drawing _1
buildWaits :: Tile -> [(Tile, Wait)]
buildWaits t = (t, Left t) : catMaybes
    [ do -- kanchan up
        nx  <- succMay t
        nx' <- succMay nx
        return (nx', Right [nx])
    , do -- kanchan down
        pr  <- predMay t
        pr' <- predMay pr
        return (pr', Right [pr])
    , do -- penchan middle down
        nx <- succMay t
        pr <- predMay t
        maybe (Just (nx, Right [pr])) (const Nothing) (succMay nx)
    , do -- penchan end down
        pr <- predMay t
        pr' <- predMay pr
        maybe (Just (pr, Right [pr'])) (const Nothing) (succMay t)
    , do -- penchan middle up
        pr <- predMay t
        nx <- succMay t
        maybe (Just (pr, Right [nx])) (const Nothing) (predMay pr)
    , do -- penchan end up
        nx <- succMay t
        nx' <- succMay nx
        maybe (Just (nx, Right [nx'])) (const Nothing) (predMay t)
    , do -- ryanmen up
        nx <- succMay t
        pr <- predMay t
        nx' <- succMay nx
        return (nx, Right [pr, nx'])
    , do -- ryanmen down
        pr <- predMay t
        pr' <- predMay pr
        nx <- succMay t
        return (pr, Right [pr', nx])
    ]
