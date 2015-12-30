{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Mahjong.Hand.Mentsu
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
------------------------------------------------------------------------------
module Mahjong.Hand.Mentsu
    (
    -- * Mentsu
    Mentsu(..), MentsuKind(..),
    toMentsu, shuntsu, koutsu, kantsu, jantou,
    shuntsuWith, fromShout, promoteToKantsu,

    -- * Functions
    mentsuTiles, mentsuShouted,
    isJantou, isShuntsu, isKoutsu, isKantsu,

    -- * Shouts
    Shout(..), ShoutKind(..),
    possibleShouts, shoutPrecedence, shoutGE

    ) where

import           Import
import           Mahjong.Tiles

-- Types

-- | For Shuntsu, the tile is the /first/ tile in chronological order.
--
-- TODO: we miss aka etc. flags here.
data Mentsu = Mentsu
    { mentsuKind :: MentsuKind
    , mentsuTiles :: [Tile] -- always in order
    , mentsuShout :: Maybe Shout
    } deriving (Show, Read {-, Eq, Ord -})

data MentsuKind = Shuntsu -- ^ 3 Tile straight
                | Koutsu -- ^ Triplet
                | Kantsu -- ^ Quadret
                | Jantou -- ^ Pair
                deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Instances

instance Pretty Mentsu where
    pretty = list . intersperse "-" . map pretty . mentsuTiles

instance Pretty [Mentsu] where
    pretty = list . intersperse "\n" . map pretty

-- | A mentsu can result from a shout; and a shout always produces
-- a mentsu.
data Shout = Shout
           { shoutKind :: ShoutKind
           , shoutFrom :: Kaze
           , shoutTile :: Tile
           , shoutTo :: [Tile]
           } deriving (Show, Read)

-- | Note: Ord instance is used to determine calling order.
data ShoutKind = Chi | Kan | Pon | Ron | Chankan
               deriving (Show, Read, Eq, Ord)

instance Pretty Shout where
    pretty s = case shoutKind s of
        Pon -> "Pon!"
        Ron -> "Ron!"
        Kan -> "Kan!"
        Chi -> "Chi!"
        Chankan -> "Chankan!"

-- Helpers

mentsuShouted :: Mentsu -> Bool
mentsuShouted = isJust . mentsuShout

-- Construct

toMentsu :: MentsuKind -> Tile -> [Tile] -> Mentsu
toMentsu mk t ts = Mentsu mk (sortOn TileEq $ t:ts) Nothing

shuntsu, koutsu, kantsu, jantou :: Tile -> Mentsu
shuntsu = mentsuFromKind Shuntsu
koutsu  = mentsuFromKind Koutsu
kantsu  = mentsuFromKind Kantsu
jantou  = mentsuFromKind Jantou

mentsuFromKind :: MentsuKind -> Tile -> Mentsu
mentsuFromKind mk t = case mk of
    Shuntsu -> Mentsu mk (t : catMaybes [succMay t, succMay t >>= succMay]) Nothing
    Koutsu  -> Mentsu mk (replicate 3 t) Nothing
    Kantsu  -> Mentsu mk (replicate 4 t) Nothing
    Jantou  -> Mentsu mk (replicate 2 t) Nothing

fromShout :: Shout -> Mentsu
fromShout s@Shout{..} = Mentsu mk (sortOn TileEq $ shoutTile : shoutTo) (Just s)
  where mk = case shoutKind of
               Pon     -> Koutsu
               Kan     -> Kantsu
               Chi     -> Shuntsu
               -- Ron and Chankan:
               _ | [_]    <- shoutTo                           -> Jantou
                 | [x, y] <- shoutTo, x ==~ y                  -> Koutsu
                 | Just m <- shuntsuWith (shoutTile : shoutTo) -> Shuntsu
                 | otherwise                                   -> error "fromShout: malformed shout"

-- | @shuntsuWith tiles@ attempts to build a shuntsu from `tiles`. Note
-- that `tiles` __must be in order of succession__.
shuntsuWith :: [Tile] -> Maybe Mentsu
shuntsuWith = go . sortOn TileEq where
    go [x, y, z] = shuntsu x <$ do succMay x >>= guard . (==~ y)
                                   succMay y >>= guard . (==~ z)
    go _ = Nothing

-- | Promote an open koutsu to a shouminkantsu
promoteToKantsu :: Mentsu -> Mentsu
promoteToKantsu (Mentsu Koutsu t s) = Mentsu Kantsu t s
promoteToKantsu _ = error "shouminkan: argument was not a koutsu"

-- Check

isJantou, isShuntsu, isKoutsu, isKantsu :: Mentsu -> Bool
isJantou  = (== Jantou)  . mentsuKind
isShuntsu = (== Shuntsu) . mentsuKind
isKoutsu  = (== Koutsu)  . mentsuKind
isKantsu  = (== Kantsu)  . mentsuKind

-- On shouts

possibleShouts :: Tile -> [(MentsuKind, [Tile])]
possibleShouts x = (Koutsu, [x, x]) : (Kantsu, [x, x, x]) : (Jantou, [x]) : if' (isSuited x) shuntsuShouts []
  where
    shuntsuShouts = catMaybes
        [ succMay x >>= \y -> succMay y >>= \z -> return (Shuntsu, [y, z]) --  x . .
        , predMay x >>= \y -> succMay x >>= \z -> return (Shuntsu, [y, z]) --  . x .
        , predMay x >>= \y -> predMay y >>= \z -> return (Shuntsu, [y, z])] --  . . x

-- | Which shout takes precedence. Can be EQ.
--
-- @shoutPrecedence target a b@
shoutPrecedence :: Kaze -- ^ shout from
                -> (Kaze, Shout)
                -> (Kaze, Shout)
                -> Ordering
shoutPrecedence dk (k, s) (k', s') = case comparing shoutKind s s' of
    EQ | shoutKind s `elem` [Ron, Chankan] -> EQ -- all winning shouts are equal
       | nextKaze dk == k                  -> GT -- a right after target
       | nextKaze dk == k'                 -> LT -- b right after target
       | prevKaze dk == k                  -> LT -- a before target
       | prevKaze dk == k'                 -> GT -- b before target
       | otherwise                         -> EQ -- a == b
    other                                  -> other

-- | @shoutGE turn_kaze a b@: a greater-or-same-precedence-as b
shoutGE :: Kaze -> (Kaze, Shout) -> (Kaze, Shout) -> Bool
shoutGE dk a b = shoutPrecedence dk a b `elem` [EQ, GT]
