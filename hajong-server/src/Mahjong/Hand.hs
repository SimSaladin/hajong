{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Hand
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This module provides the representation type for a mahjong hand
-- (@Hand@), and functions that operate on a hand.
------------------------------------------------------------------------------
module Mahjong.Hand
    ( module Mahjong.Hand
    , module Mahjong.Hand.Mentsu
    , module Mahjong.Hand.Value
    ) where

import qualified Data.List as L

import Mahjong.Hand.Mentsu
import Mahjong.Hand.Value
import Mahjong.Hand.Algo
import Mahjong.Hand.Yaku
import Mahjong.Tiles
import Mahjong.State

-- | Hide private info from the data type.
maskPublicHand :: Hand -> Hand
maskPublicHand = (handConcealed .~ []) . (handPick .~ Nothing)
               . (handFuriten .~ Nothing)  . (hCanTsumo .~ False)

-- * Draw

toHand :: CanError m => Tile -> Hand -> m Hand
toHand t = do
    h <- handPick .~ Just t
    return $ return $ if' (complete h) (hCanTsumo .~ True) id h

toHandWanpai :: CanError m => Tile -> Hand -> m Hand
toHandWanpai t h = do
    unless (h^.handPublic.handDrawWanpai) (throwError "Cannot draw from wanpai")
    toHand t h <&> handPublic.handDrawWanpai .~ False

-- * Discard

-- | Discard a tile; fails if
--
--  1. tile not in the hand
--  2. riichi restriction
--  3. need to draw first
discard :: CanError m => Discard -> Hand -> m Hand
discard d@Discard{..} hand
    | _dcRiichi && hand ^. handPublic.handRiichi      = throwError "Already in riichi"
    | hand^.handPublic.handDrawWanpai || canDraw hand = throwError "You need to draw first"
    | _dcRiichi && not (canRiichiWith _dcTile hand)   = throwError "Cannot riichi: not tenpai"
    | hand^.handPick /= Just _dcTile
    , hand^.handPublic.handRiichi                     = throwError "Cannot change wait in riichi"
    | otherwise = setRiichi . setNoTsumo . movePick . setDiscard <$> tileFromHand _dcTile hand
  where
    movePick h
        | Just p <- _handPick h = h & (handConcealed %~ (`snoc` p)) . (handPick .~ Nothing)
        | otherwise             = h
    setDiscard = handPublic.handDiscards %~ (|> d)
    setNoTsumo = hCanTsumo .~ False
    setRiichi
        | _dcRiichi = handPublic.handRiichi .~ True
        | otherwise = id

-- | Automatically execute a discard necessary to advance the game (in case
-- of inactive players).
handAutoDiscard :: CanError m => Hand -> m Discard
handAutoDiscard hand
    | Just tile <- _handPick hand = return $ Discard tile Nothing False
    | otherwise                   = return $ Discard (hand ^?! handConcealed._last) Nothing False

-- * Winning

handWin :: CanError m => Maybe Shout -> Hand -> m Hand
handWin ms h
    | not (complete h)       = throwError "Cannot win with an incomplete hand"
    | isJust ms && furiten h = throwError "You are furiten"
    | otherwise              = return $ setAgari ms h

-- * Kan

-- | Ankan on the given tile if possible.
--
-- Remember to flip dora when this succeeds.
ankanOn :: CanError m => Tile -> Hand -> m Hand
ankanOn tile hand
    | [_,_,_,_] <- sameConcealed  = return hand'
    | [_,_,_]   <- sameConcealed
    , hand^.handPick == Just tile = return $ hand' & handPick .~ Nothing
    | otherwise                   = throwError "Not enough same tiles"
    where
        sameConcealed = hand^.handConcealed^..folded.filtered (== tile)
        hand'         = hand & handConcealed %~ filter (/= tile)
                             & handPublic.handCalled %~ (:) (kantsu tile)
                             & handPublic.handDrawWanpai .~ True

-- | Shouminkan with the tile if possible.
--
-- Remember to flip dora when this succeeds.
shouminkanOn :: CanError m => Tile -> Hand -> m Hand
shouminkanOn tile hand = do
    hand' <- tile `tileFromHand` hand
    let isShoum m = mentsuKind m == Koutsu && mentsuTile m == tile
    case hand' ^? handPublic.handCalled.each.filtered isShoum of
        Nothing -> throwError "Shouminkan not possible: no such open koutsu"
        Just _  -> return $ handPublic.handCalled.each.filtered isShoum %~ promoteToKantsu $ hand'

-- * Call

-- | Meld the mentsu to the hand
meldTo :: CanError m => Shout -> Mentsu -> Hand -> m Hand
meldTo shout mentsu hand
    | hand^.handConcealed.to (\xs -> length ih + length (xs L.\\ ih) == length xs)
    = if' (shoutKind shout == Ron) (handWin $ Just shout) return
    $ if' (shoutKind shout == Kan) (handPublic.handDrawWanpai .~ True) id
    $ handPublic.handCalled %~ (|> mentsu)
    $ handConcealed         %~ (L.\\ ih)
    $ hand
    | otherwise = throwError "meldTo: Tiles not available"
  where ih = shoutTo shout

-- | Transfer the discard from the hand to a mentsu specified by the shout.
shoutFromHand :: CanError m => Kaze -> Shout -> Hand -> m (Mentsu, Hand)
shoutFromHand sk shout hand =
    case hand ^? handPublic.handDiscards._last of
        Nothing          -> throwError "Player hasn't discarded anything"
        Just Discard{..} -> do
            isJust _dcTo `when` throwError "The discard has already been claimed"
            (shoutTile shout /= _dcTile) `when` throwError "The discard is not the shouted tile"
            return (fromShout shout,
                   hand & handPublic.handDiscards._last.dcTo .~ Just sk)

-- * Valued hand

valueHand :: Kaze -> Hand -> Deal -> ValuedHand
valueHand player h deal = ValuedHand (h^.handPublic.handCalled) (h^.handConcealed) (getValue vi)
  where vi = ValueInfo
            (deal^.pRound)
            player
            (h^.handPublic.handRiichi)
            (h^.handPublic.handCalled.to null)
            (h^.handPublic.handDiscards^..each.dcTile)
            (h^.handPublic.handCalled)
            (h^.handPublic.handAgari.to (?! "handAgari not set"))
            (h^.handPublic.handAgariCall)
            (h^.handConcealed ++ maybe [] return (h^.handPick))
            (h^.handPublic.hIppatsu)
            (h^.handPublic.hDoubleRiichi)
            (deal^.pWallTilesLeft)
            (h^.handPublic.hLastFromWanpai)

-- * Utility

-- | All mentsu that could be melded with hand given some tile.
shoutsOn :: Kaze -- ^ Shout from (player in turn)
         -> Tile -- ^ Tile to shout
         -> Kaze -- ^ Shouter
         -> Hand -- ^ shouter's
         -> [Shout]
shoutsOn np t p hand
    | np == p   = [] -- You're not shouting the thing you just discarded from yourself, right?
    | otherwise = concatMap toShout $ possibleShouts (nextKaze np == p) t
  where
    ih                = sort (_handConcealed hand) -- NOTE: sort
    toShout (mk, xs)  = do
        guard $ xs `isInfixOf` ih
        s <- case mk of
                Jantou  -> [Ron]
                Kantsu  -> [Kan]
                Koutsu  -> [Pon, Ron]
                Shuntsu -> [Chi, Ron]
        when (hand^.handPublic.handRiichi) $ guard (s == Ron)
        guard $ if s == Ron
                then complete
                    ( toMentsu mk t xs : (hand^.handPublic.handCalled), _handConcealed hand L.\\ xs )
                else mk /= Jantou
        return $ Shout s np t xs

-- | From wall (not wanpai).
canDraw :: Hand -> Bool
canDraw h = not (h^.handPublic.handDrawWanpai)
    && isNothing (h^.handPick)
    && (3 * length (h^.handPublic.handCalled) + length (h^.handConcealed) == 13)

-- | Tiles the hand can discard for a riichi.
handCanRiichiWith :: Hand -> [Tile]
handCanRiichiWith h
    | h^.handPublic.handRiichi = []
    | otherwise                = h^.handConcealed.to (mapMaybe f)
    where f t = guard (canRiichiWith t h) >> return t

canRiichiWith :: Tile -> Hand -> Bool
canRiichiWith t h = null (h^.handPublic.handCalled) && tenpai (L.delete t tiles)
    where tiles = h^.handConcealed ++ maybe [] return (h^.handPick)

furiten :: Hand -> Bool
furiten h = any (`elem` (h^..handPublic.handDiscards.each.dcTile)) . concatMap getAgari
          . filter tenpai $ getGroupings h

-- | If there is a shuntsu wait, that is the only possible agari. If there
-- is a single leftover tile that is the agari. otherwise any of the koutsu
-- waits can be completed (thus agari).
--
--  TODO: chiitoitsu etc missing
getAgari :: Grouping -> [Tile]
getAgari xs | [GroupWait Shuntsu _ ws] <- filter isShuntsuWait xs = ws
            | [t] <- leftovers xs                                 = [t]
            | otherwise                                           = concatMap (either return id) $ waits xs

isShuntsuWait :: TileGroup -> Bool
isShuntsuWait (GroupWait Shuntsu _ _) = True
isShuntsuWait _                       = False

setAgari :: Maybe Shout -> Hand -> Hand
setAgari ms h = h & handPublic.handAgari .~ mt & handPublic.handAgariCall .~ mc 
    where (mt, mc) = case ms of
                         Just s -> (Just $ shoutTile s, Just s)
                         Nothing -> (h^.handPick, Nothing)

-- | Take the tile from hand if possible
tileFromHand :: CanError m => Tile -> Hand -> m Hand
tileFromHand tile hand
    | Just tile' <- hand ^. handPick, tile == tile'         = return $ handPick .~ Nothing $ hand
    | (xs, _ : ys) <- break (== tile) (_handConcealed hand) = return $ handConcealed .~ (xs ++ ys) $ hand
    | otherwise                                             = throwError "Tile not in hand"
