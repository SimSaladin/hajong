------------------------------------------------------------------------------
-- | 
-- Module         : Hand
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hand where

import ClassyPrelude
import Control.Lens
import Tiles

newtype Player = Player Kazehai deriving (Show, Read, Eq, Ord)
deriving instance Enum Player

data HandPublic = HandPublic
                { _handOpen :: [Mentsu]
                , _handDiscards :: [(Tile, Maybe Player)]
                , _handRiichi :: Bool
                } deriving (Show, Read, Eq)

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handPick :: Maybe Tile
          , _handFuriten :: Maybe Bool -- ^ Just (temporary?)
          , _handPublic :: HandPublic
          } deriving (Show, Read, Eq)

makeLenses ''HandPublic
makeLenses ''Hand

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> Hand
initHand tiles = Hand tiles Nothing Nothing $ HandPublic [] [] False

-- | Discard a tile; returns Left if discard is not possible due to the
-- tile 1) not being in the hand or 2) due to riichi restriction.
discard :: Tile -> Hand -> Either Text Hand
discard tile hand
    | hand ^. handPick == Just tile = Right $ hand & set handPick Nothing . setDiscard
    | hand ^. handPublic.handRiichi = Left "Cannot change wait in riichi"
    | otherwise                     = case ys of
        []      -> Left "Tile not in hand"
        (_:ys') -> Right $ hand & set handConcealed (xs ++ ys') . setDiscard
    where
        (xs, ys) = break (== tile) (_handConcealed hand)
        setDiscard = over (handPublic.handDiscards) (++ [(tile, Nothing)])

-- | Left for 1) already in riichi or 2) not tenpai.
setRiichi :: Tile -> Hand -> Either Text Hand
setRiichi tile hand
    | hand ^. handPublic.handRiichi = Left "Already in riichi"
    | tenpai hand                   = Right $ set (handPublic.handRiichi) True hand
    | otherwise                     = Left "Not in tenpai"

tenpai :: Hand -> Bool
tenpai hand = True -- TODO implement
