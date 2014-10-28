------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Hand
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This module provides the representation type for a mahjong hand
-- (@Hand@), and functions that operate on a hand.
------------------------------------------------------------------------------
module Mahjong.Hand where

import qualified Data.List as L

import Mahjong.Hand.Mentsu
import Mahjong.Hand.Algo
import Mahjong.Tiles

-- * Types

data HandPublic = HandPublic
                { _handOpen :: [Mentsu]
                , _handDiscards :: [(Tile, Maybe Kaze)]
                , _handRiichi :: Bool
                , _handTurnDiscard :: Maybe (Tile, UTCTime)
                } deriving (Show, Read, Eq)

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handPick :: Maybe Tile
          , _handFuriten :: Maybe Bool -- ^ Just (temporary?)
          , _handPublic :: HandPublic
          } deriving (Show, Read, Eq)

makeLenses ''HandPublic
makeLenses ''Hand

instance HasGroupings Hand where
    getGroupings h = getGroupings $ (,) <$> _handOpen . _handPublic <*> _handConcealed $ h

-- * Create

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> Hand
initHand tiles = Hand tiles Nothing Nothing $ HandPublic [] [] False Nothing

-- * Discarding

-- | Discard a tile; returns Left if discard is not possible due to the
-- tile 1) not being in the hand or 2) due to riichi restriction.
discard :: CanError m => Tile -> Hand -> m Hand
discard tile hand
    | hand ^. handPick == Just tile = return $ hand & set handPick Nothing . setDiscard
    | hand ^. handPublic.handRiichi = throwError "Cannot change wait in riichi"
    | [] <- ys                       = throwError "Tile not in hand"
    | Just pick <- hand^.handPick
    , (_:ys')   <- ys                = return
        . set handPick Nothing
        . set handConcealed (pick : xs ++ ys')
        $ setDiscard hand
    | otherwise                     = throwError "No draw done"
    where
        (xs, ys)   = break (== tile) (_handConcealed hand)
        setDiscard = (handPublic.handDiscards) %~ (++ [(tile, Nothing)])

-- | Do riichi if possible and discard.
discardRiichi :: CanError m => Tile -> Hand -> m Hand
discardRiichi tile hand
    | hand ^. handPublic.handRiichi = throwError "Already in riichi"
    | tenpai hand                   = discard tile $ handPublic.handRiichi.~True $ hand
    | otherwise                     = throwError "Not in tenpai"

-- | Automatically execute a discard necessary to advance the game (in case
-- of inactive players).
handAutoDiscard :: Hand -> Tile
handAutoDiscard hand
    | Just tile <- _handPick hand = tile
    | otherwise                   = error "handAutoDiscard: nothing to do"
        -- TODO what if he just ankan with the discard?
        -- .. wait, that should bring automatically a new pick to his hand
        -- from wanpai.

-- * Properties

-- | All mentsu that could be melded with hand given some tile.
shoutsOn :: Tile -> Hand -> [Shout]
shoutsOn tile hand = [] -- TODO

-- * Melding

-- | Do an ankan on the given tile.
ankanOn :: CanError m => Tile -> Hand -> m Hand
ankanOn tile hand 
    | [_,_,_,_] <- sameConcealed   = return hand'
    | [_,_,_]   <- sameConcealed
    , hand^.handPick == Just tile = return $ hand' & handPick .~ Nothing
    | otherwise                   = throwError "Not enough same tiles"
    where
        sameConcealed = hand^.handConcealed^..folded.filtered (== tile)
        hand'         = hand & handConcealed %~ filter (/= tile)
                             & handPublic.handOpen %~ (:) (kantsu tile)

-- | Meld the mentsu to the hand
meldTo :: CanError m => Shout -> Mentsu -> Hand -> m Hand
meldTo shout mentsu hand
    | hand^.handConcealed.to (\xs -> length todelete + length (xs L.\\ todelete) == length xs)
    = return $ handPublic.handOpen %~ (|> mentsu)
             $ handConcealed %~ (L.\\ todelete)
             $ hand
    | otherwise = throwError "Tiles not available"
    where todelete = ($ shout) $ case shout of
             Pon{} -> replicate 2 . shoutedTile 
             Kan{} -> replicate 3 . shoutedTile
             _ -> shoutedTo

-- | Transfer the discard from the hand to a mentsu specified by the shout.
shoutFromHand :: CanError m => Shout -> Hand -> m (Mentsu, Hand)
shoutFromHand shout hand
    | Just (tile, _) <- hand^.handPublic.handTurnDiscard
    , shoutedTile shout == tile
    = return (fromShout shout, hand & handPublic.handTurnDiscard .~ Nothing
                                    & handPublic.handDiscards %~ shoutLastDiscard shout)
    | otherwise = throwError "The hand has no discard to take."

-- | Modify the last discard in the discard pile to indicate a shout.
shoutLastDiscard :: Shout -> [(Tile, Maybe Kaze)] -> [(Tile, Maybe Kaze)]
shoutLastDiscard     _     [] = error "shoutLastDiscard: empty list"
shoutLastDiscard shout (x:xs) = go x xs
    where go (tile, _) [] = [(tile, Just $ shoutedFrom shout)]
          go d     (y:ys) = d : go y ys
