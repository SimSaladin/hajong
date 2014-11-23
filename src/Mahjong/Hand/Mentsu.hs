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
    possibleShouts, shoutPrecedence

    ) where

import Mahjong.Tiles
import Text.PrettyPrint.ANSI.Leijen (Pretty(..))

-- Types

-- | For Shuntsu, the tile is the /first/ tile in chronological order.
data Mentsu = Mentsu
    { mentsuKind :: MentsuKind
    , mentsuTile :: Tile
    , mentsuShout :: Maybe Shout
    } deriving (Show, Read, Eq, Ord)

data MentsuKind = Shuntsu -- ^ 3 Tile straight
                | Koutsu -- ^ Triplet
                | Kantsu -- ^ Quadret
                | Jantou -- ^ Pair
                deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Instances

instance Pretty Mentsu where
    pretty = intercalate "-" . map pretty . mentsuTiles

instance Pretty [Mentsu] where
    pretty = intercalate "\n" . map pretty

-- | A mentsu can result from a shout; and a shout always produces
-- a mentsu.
data Shout = Shout
           { shoutKind :: ShoutKind
           , shoutedFrom :: Kaze
           , shoutedTile :: Tile
           , shoutedTo :: [Tile]
           } deriving (Show, Read, Eq, Ord)

-- | Note: Ord instance is used to determine calling order.
data ShoutKind = Ron | Kan | Pon | Chi
               deriving (Show, Read, Eq, Ord)

instance Pretty Shout where
    pretty s = case shoutKind s of
        Pon -> "Pon!"
        Ron -> "Ron!"
        Kan -> "Kan!"
        Chi -> "Chi!"

-- Helpers

mentsuTiles :: Mentsu -> [Tile]
mentsuTiles Mentsu{..} = case mentsuKind of
    Shuntsu -> mentsuTile : catMaybes [succMay mentsuTile, succMay mentsuTile >>= succMay]
    Koutsu  -> replicate 3 mentsuTile
    Kantsu  -> replicate 4 mentsuTile
    Jantou  -> replicate 2 mentsuTile

mentsuShouted :: Mentsu -> Bool
mentsuShouted = isJust . mentsuShout

-- Construct

toMentsu :: MentsuKind -> Tile -> [Tile] -> Mentsu
toMentsu mk t ts = case mk of
    Shuntsu -> shuntsu $ headEx $ sort (t : ts)
    Koutsu -> koutsu t
    Kantsu -> kantsu t
    Jantou -> jantou t

shuntsu, koutsu, kantsu, jantou :: Tile -> Mentsu
shuntsu = Mentsu Shuntsu `flip` Nothing
koutsu  = Mentsu Koutsu `flip` Nothing
kantsu  = Mentsu Kantsu `flip` Nothing
jantou  = Mentsu Jantou `flip` Nothing

fromShout :: Shout -> Mentsu
fromShout s@Shout{..} = setShout $ case shoutKind of
    Pon -> koutsu shoutedTile
    Kan -> kantsu shoutedTile
    Chi -> shuntsu (minimumEx $ shoutedTile : shoutedTo)
    Ron
        | [_]   <- shoutedTo         -> jantou shoutedTile
        | [x,y] <- shoutedTo, x == y -> koutsu shoutedTile
        | Just m <- shuntsuWith (shoutedTile : shoutedTo) -> m
        | otherwise -> error "fromShout: malformed shout"
    where
        setShout (Mentsu k t _) = Mentsu k t (Just s)

-- | @shuntsuWith tiles@ attempts to build a shuntsu from `tiles`. Note
-- that `tiles` __must be in order of succession__.
shuntsuWith :: [Tile] -> Maybe Mentsu
shuntsuWith [x, y, z] = shuntsu x <$ do
    succMay x >>= guard . (== y)
    succMay y >>= guard . (== z)
shuntsuWith _ = Nothing

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

possibleShouts :: Bool -> Tile -> [(MentsuKind, [Tile])]
possibleShouts withShuntsu x = (Koutsu, [x, x])
    : (Kantsu, [x, x, x])
    : (Jantou, [x])
    : (if withShuntsu then shuntsus else [])
  where
    shuntsus = catMaybes
        [ succMay x >>= \y -> succMay y >>= \z -> return (Shuntsu, [y, z]) --  x . .
        , predMay x >>= \y -> succMay x >>= \z -> return (Shuntsu, [y, z]) --  . x .
        , predMay x >>= \y -> predMay y >>= \z -> return (Shuntsu, [y, z]) --  . . x
        ]

-- | Which shout takes precedence. Never @EQ@, always 'GT' or 'LT'.
shoutPrecedence :: Kaze -- ^ shout from
                -> (Kaze, Shout)
                -> (Kaze, Shout)
                -> Ordering
shoutPrecedence dk (k, s) (k', s') = case comparing shoutKind s s' of
    EQ | nextKaze dk == k  -> GT
       | nextKaze dk == k' -> LT
       | prevKaze dk == k  -> LT
       | prevKaze dk == k' -> GT
       | otherwise         -> error "shoutPrecedence: undecidable, this is not possible"
    other                  -> other
