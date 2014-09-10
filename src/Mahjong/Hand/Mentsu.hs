------------------------------------------------------------------------------
-- | 
-- Module         : Mahjong.Hand.Mentsu
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
------------------------------------------------------------------------------
module Mahjong.Hand.Mentsu
    (
    -- * Build

    -- ** From a tile
    shuntsu, koutsu, kantsu, jantou
    -- ** From a shout
    , fromShout
    -- ** With guards
    , shuntsuWith

    -- * Functions
    , mentsuKind
    , mentsuTiles
    , mentsuShout
    , isJantou, isShuntsu, isKoutsu, isKantsu

    -- * Types
    , Mentsu(..), MentsuKind(..), Shout(..)
    ) where

import Mahjong.Tiles

-- Types

-- | For Shuntsu, the tile is the /first/ tile in chronological order.
data Mentsu = Mentsu MentsuKind Tile (Maybe Shout)
            deriving (Show, Read, Eq, Ord)

data MentsuKind = Shuntsu -- ^ 3 Tile straight
                | Koutsu -- ^ Triplet
                | Kantsu -- ^ Quadret
                | Jantou -- ^ Pair
                deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | A mentsu can result from a shout; and a shout always produces
-- a mentsu.
data Shout = Pon { shoutedFrom :: Player, shoutedTile :: Tile }
           | Kan { shoutedFrom :: Player, shoutedTile :: Tile }
           | Chi { shoutedFrom :: Player, shoutedTile :: Tile, shoutedTo :: [Tile] }
           | Ron { shoutedFrom :: Player, shoutedTile :: Tile, shoutedTo :: [Tile] }
           deriving (Show, Read, Eq, Ord)

-- | Get the mentsu kind
mentsuKind :: Mentsu -> MentsuKind
mentsuKind (Mentsu k _ _) = k

mentsuTiles :: Mentsu -> [Tile]
mentsuTiles (Mentsu mk t _) = case mk of
                                  Shuntsu -> t : catMaybes [succMay t, succMay t >>= succMay]
                                  Koutsu  -> replicate 3 t
                                  Kantsu  -> replicate 4 t
                                  Jantou  -> replicate 2 t

mentsuShout :: Mentsu -> Maybe Shout
mentsuShout (Mentsu _ _ x) = x

-- * Construct

shuntsu, koutsu, kantsu, jantou :: Tile -> Mentsu
shuntsu = Mentsu Shuntsu `flip` Nothing
koutsu  = Mentsu Koutsu `flip` Nothing
kantsu  = Mentsu Kantsu `flip` Nothing
jantou  = Mentsu Jantou `flip` Nothing

fromShout :: Shout -> Mentsu
fromShout shout = setShout $ case shout of
    Pon{} -> koutsu (shoutedTile shout)
    Kan{} -> kantsu (shoutedTile shout)
    Chi{} -> shuntsu (minimumEx $ shoutedTile shout : shoutedTo shout)
    Ron{}
        | [_]   <- shoutedTo shout         -> jantou (shoutedTile shout)
        | [x,y] <- shoutedTo shout, x == y -> koutsu (shoutedTile shout)
        | otherwise                       -> error "Impossible ron to a kantsu!"
    where
        setShout (Mentsu k t _) = Mentsu k t (Just shout)

-- | @shuntsuWith tiles@ attempts to build a shuntsu from `tiles`. Note
-- that `tiles` __must be in order of succession__.
shuntsuWith :: [Tile] -> Maybe Mentsu
shuntsuWith (x:y:z:[]) = shuntsu x <$ do
    succMay x >>= guard . (== y)
    succMay y >>= guard . (== z)
shuntsuWith          _ = Nothing

-- * Check

isJantou, isShuntsu, isKoutsu, isKantsu :: Mentsu -> Bool
isJantou  = (== Jantou)  . mentsuKind
isShuntsu = (== Shuntsu) . mentsuKind
isKoutsu  = (== Koutsu)  . mentsuKind
isKantsu  = (== Kantsu)  . mentsuKind
