{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Mahjong.Hand.Algo.WaitTree
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Hand.Algo.WaitTree
    (
    -- * WaitTree
    WaitTree, DevOp(..), TenpaiOp(..)
    , buildGWTs', buildGWTs, buildGWT, devops

    -- * RootedTree
    , RootedTree(..), RootedBranch(..)
    -- ** Functions
    , minDepth, flatten
    -- ** Unfolding
    , unfoldRootedTree, unfoldRootedBranch
    ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.List.HT (removeEach)
import           Data.Either (isLeft)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Mahjong.Tiles
import Mahjong.Hand.Mentsu
import Mahjong.Hand.Algo

-- * WaitTree

-- | A @WaitTree@ models the development options of an mahjong hand.
type WaitTree = RootedTree Grouping DevOp TenpaiOp

-- | Development operation that waits for tile `waitFor` discarding
-- `waitDiscard`.
data DevOp = DevOp { opDiscard  :: Tile
                   , opFor      :: Tile
                   } deriving (Eq, Ord, Show)

-- | Non-recursive helper type for the unfolder below.
--
-- Left for ready with, Right for (discarding, getting, stray tiles, waits).
type DevOp' = Either Tile (Tile, Tile, [Tile], [Wait])

-- | Getting the tile results in a win.
data TenpaiOp = TenpaiOp Tile Int -- TODO last filed should be Value
              deriving (Eq, Ord, Show)

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
buildGWTs = map buildGWT . getGroupings

-- | @buildGWT group@ discards groupings strictly less than shanten over
-- `groups` before building the tree.
buildGWT :: Grouping -> WaitTree
buildGWT g = unfoldRootedTree g go (toList $ devops <$> leftovers <*> waits $ g)
    where
        go :: DevOp' -> Either TenpaiOp (DevOp, NonEmpty DevOp')
        go = bimap (`TenpaiOp` 0)
                   (\(disc, draw, lo, wa) -> (DevOp disc draw, devops lo wa))

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
devops :: [Tile] -> [Wait] -> NonEmpty DevOp'
devops f_ts w_ts
    | []  <- f_ts, []  <- w_ts     = error "devops: called with a malformed hand (all mentsu)"
    | []  <- f_ts, [_] <- w_ts     = error "devops: called with a malformed hand (one mentsu free only)"
    | []  <- f_ts, tenpaiWaits     = NE.fromList tenpaiHasKoutsu
    | []  <- f_ts                  = NE.fromList breakingWait
    | [x] <- f_ts, []  <- w_ts     = return (Left x) -- Last pair wait
    |              []  <- w_ts     = NE.fromList leftoversOnly
    | otherwise                    = NE.fromList meldLeftovers
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

-- | Get the tiles in hand of the shuntsu wait based on the tiles waited.
shuntsuWaitToHandTiles :: [Tile] -> [Tile]
shuntsuWaitToHandTiles [] = error "shuntsuWaitToHandTiles: empty list of waits"
shuntsuWaitToHandTiles [t] = catMaybes $ case tileNumber t of
    Just San -> [predMay t, predMay t >>= predMay]
    Just Chii -> [succMay t, succMay t >>= succMay]
    _ -> [predMay t, succMay t]
shuntsuWaitToHandTiles (t:_) = catMaybes [succMay t, succMay t >>= succMay]

-- Instances

instance Pretty DevOp where
    pretty (DevOp disc draw) = pretty disc ++ " for " ++ pretty draw

instance Pretty TenpaiOp where
    pretty (TenpaiOp draw val) = "Tenpai for " ++ pretty draw ++ " at " ++ PP.int val ++ " fu"


-- * RootedTree

-- | A rose tree with a single root of type `root`, leaves of type `leaf`
-- and inner data of type `inner`.
data RootedTree root inner leaf = RootedTree root [RootedBranch inner leaf]

data RootedBranch inner leaf = RootedLeaf leaf
                             | RootedBranch inner (NonEmpty (RootedBranch inner leaf))

-- | Build a tree from root and a seed value
unfoldRootedTree :: r -> (b -> Either l (i, NonEmpty b)) -> [b] -> RootedTree r i l
unfoldRootedTree root f seed = RootedTree root (map (unfoldRootedBranch f) seed)

unfoldRootedBranch :: (b -> Either l (i, NonEmpty b)) -> b -> RootedBranch i l
unfoldRootedBranch f = go
  where
    go = either RootedLeaf (\(i,bs) -> RootedBranch i (go <$> bs)) . f

-- | Minimum depth. 0 when no branches.
minDepth :: RootedTree r i l -> Int
minDepth (RootedTree _ xs) = fromMaybe 0 $ minimumMay (go <$> xs)
    where
        go (RootedLeaf _)      = 1
        go (RootedBranch _ bs) = 1 + (fromMaybe 0 (minimumMay (go <$> bs)))

-- | Nodes on every level /excluding/ root.
flatten :: RootedTree r i l -> [[Either i l]]
flatten (RootedTree _ []) = []
flatten (RootedTree _ branches) = foldl' go [] branches
    where
        go :: [[Either i l]] -> RootedBranch i l -> [[Either i l]]
        go []                       x = go [[]] x
        go (x:xs) (RootedLeaf l)      = (Right l : x) : xs
        go (x:xs) (RootedBranch i bs) = (Left i : x) : foldl' go xs bs

-- Instances

instance Bifunctor (RootedTree root) where
    bimap f g (RootedTree root xs) = RootedTree root (bimap f g <$> xs)

instance Bifunctor RootedBranch where
    bimap f g (RootedBranch inner xs) = RootedBranch (f inner) (bimap f g <$> xs)
    bimap _ g (RootedLeaf outer)      = RootedLeaf (g outer)

instance (Pretty r, Pretty i, Pretty l) => Pretty (RootedTree [r] i l) where -- XXX [r] is artificial convenience restriction for WaitTree
    pretty (RootedTree root branches) =
        foldl' (\x y -> x PP.<+> ":" PP.<+> y) mempty (map pretty root)
        PP.<$$> prettyList branches
        PP.<+> PP.hardline

instance (Pretty inner, Pretty leaf) => Pretty (RootedBranch inner leaf) where
    pretty (RootedLeaf leaf)       = "└╼" PP.<+> pretty leaf
    pretty (RootedBranch inner xs) =
        "└┬╼" PP.<+> pretty inner PP.<$> PP.indent 1 (PP.align (prettyList (toList xs)))

    prettyList branches = foldl' (PP.<$>) mempty (pretty <$> branches)
