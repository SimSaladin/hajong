------------------------------------------------------------------------------
-- Module         : Hajong.Game.Hand
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- A single player's hand.
------------------------------------------------------------------------------
module Hajong.Game.Hand where

import ClassyPrelude
import Control.Lens

import Hajong.Game.Types
import Hajong.Game.Tiles
import Hajong.Game.Mentsu
import Hajong.Game.Yaku.Standard

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> Hand
initHand tiles = Hand tiles Nothing Nothing $ HandPublic [] [] False Nothing

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

doAnkan :: Tile -> Hand -> Either Text Hand
doAnkan tile hand = case len of
                        4 -> Right $ handConcealed %~ filter (/= tile) $ hand
                        3   | hand ^. handPick == Just tile -> Right
                                $ handPick .~ Nothing
                                $ handConcealed %~ filter (/= tile)
                                $ hand
                            | otherwise -> Left "Cannot ankan"
                        _ -> Left "Cannot ankan"
    where len = length (hand^.handConcealed^..folded.filtered (== tile))

doShout :: Shout -> Player -> Hand -> Either Text (Hand, Player -> Hand -> Maybe (Either () Mentsu))
doShout shout shouter hand =
    case hand ^. handPublic.handTurnDiscard of
        Nothing            -> Left "Player has no recent discard"
        Just (tile, _time) -> Right
            ( (handPublic.handTurnDiscard .~ Nothing)
            . (handPublic.handDiscards %~ (<> [(tile, Just shouter)])) $ hand
            , toShout shout tile
            )

toShout :: Shout -> Tile -> Player -> Hand
        -> Maybe (Either () Mentsu) -- ^ Left stands for complete with ron
toShout shout tile player hand =
    case shout of
        Pon | fc tile == 2 -> Just $ Right $ Koutsu [tile,tile,tile] (Just player)
            | otherwise    -> Nothing
        Kan | fc tile == 3 -> Just $ Right $ Kantsu [tile,tile,tile,tile] (Just player)
            | otherwise    -> Nothing
        Ron | handComplete (hand^.handPublic.handOpen) (tile : hand^.handConcealed) -> Just $ Left ()
            | otherwise              -> Nothing
        Chi (x, y)
            | isShuntsu' [tile,x,y] && fc x > 0 && fc y > 0 -> Just $ Right $ Shuntsu (sort [tile,x,y]) (Just player)
            | otherwise                                     -> Nothing
    where
        fc t = length $ hand^.handConcealed^..folded.filtered (== t)
