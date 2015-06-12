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
    ( Hand(..), HandA, HandP
    , Discard(..)
    , RiichiState(..), DrawState(..), PickedTile(..), FuritenState(..)
    , module Mahjong.Hand
    , module Mahjong.Hand.Algo
    , module Mahjong.Hand.Mentsu
    , module Mahjong.Hand.Value
    , module Mahjong.Hand.Yaku
    -- * Lenses
    , handCalled   
    , handDiscards 
    , handRiichi   
    , handIppatsu  
    , handState    
    , handPicks    
    , handConcealed
    , handFuriten  
    , handCanTsumo 

    , dcTile, dcRiichi, dcTo
    ) where

------------------------------------------------------------------------------
import qualified Data.List as L
------------------------------------------------------------------------------
import           Import
import           Mahjong.Tiles
import           Mahjong.Kyoku.Internal
------------------------------------------------------------------------------
import           Mahjong.Hand.Algo
import           Mahjong.Hand.Mentsu
import           Mahjong.Hand.Value
import           Mahjong.Hand.Yaku
import           Mahjong.Hand.Internal
------------------------------------------------------------------------------

-- | Hide private info from the data type.
maskPublicHand :: HandA -> HandP
maskPublicHand hand =
    hand { _handPicks = map maskPickedTile (_handPicks hand)
         , _handConcealed = Nothing
         , _handFuriten = Nothing
         , _handCanTsumo = Nothing }
    where
        maskPickedTile (FromWall _)       = FromWall Nothing
        maskPickedTile (FromWanpai _)     = FromWanpai Nothing
        maskPickedTile (AgariTsumo t)     = AgariTsumo t
        maskPickedTile (AgariCall t k)    = AgariCall t k
        maskPickedTile (AgariRinshan t k) = AgariRinshan t k

convertHand :: HandA -> HandP
convertHand hand = hand { _handPicks = map convertPickedTile (_handPicks hand)
                        , _handConcealed = Just . runIdentity $ _handConcealed hand
                        , _handFuriten = Just . runIdentity $ _handFuriten hand
                        , _handCanTsumo = Just . runIdentity $ _handCanTsumo hand }
    where
        convertPickedTile (FromWall t)       = FromWall (Just $ runIdentity t)
        convertPickedTile (FromWanpai t)     = FromWanpai (Just $ runIdentity t)
        convertPickedTile (AgariTsumo t)     = AgariTsumo t
        convertPickedTile (AgariCall t k)    = AgariCall t k
        convertPickedTile (AgariRinshan t k) = AgariRinshan t k

-- * Draw

toHand :: CanError m => Tile -> HandA -> m HandA
toHand t h = do
    unless (h^.handState == DrawFromWall) $ throwError $ "Hand state was " <> tshow (h^.handState) <> ", but expected " <> tshow DrawFromWall
    return $ setPickAndTsumo (FromWall $ pure t) h

toHandWanpai :: CanError m => Tile -> HandA -> m HandA
toHandWanpai t h = do
    unless (h^.handState == DrawFromWanpai) $ throwError $ "Hand state was " <> tshow (h^.handState) <> ", but expected " <> tshow DrawFromWanpai
    return $ setPickAndTsumo (FromWanpai $ pure t) h

setPickAndTsumo :: PickedTile Identity -> HandA -> HandA
setPickAndTsumo pick h =
    let h' = handPicks %~ (++ [pick]) $ handState.~DrawNone $ h
        in if' (complete h') (handCanTsumo .~ pure True) id h'

-- * Discard

-- | Discard a tile; fails if
--
--  1. tile not in the hand
--  2. riichi restriction
--  3. need to draw first
discard :: CanError m => Discard -> HandA -> m HandA
discard d@Discard{..} hand
    | _dcRiichi, hand^.handRiichi /= NoRiichi     = throwError "Already in riichi"
    | _dcRiichi, not (canRiichiWith _dcTile hand) = throwError "Cannot riichi: not tenpai"
    | hand^.handState /= DrawNone                 = throwError "You need to draw first"

    | hand^.handRiichi /= NoRiichi
    , p : _ <- hand^.handPicks
    , pickedTile p /= _dcTile                     = throwError "Cannot change wait in riichi"

    | otherwise = setRiichi . setIppatsu . setNoTsumo . movePicks . setDiscard <$> tileFromHand _dcTile hand
  where
    movePicks h = h & handConcealed._Wrapped %~ (++ map pickedTile (_handPicks h)) & handPicks .~ []
    setDiscard  = handDiscards %~ (|> d)
    setNoTsumo  = handCanTsumo._Wrapped .~ False
    setRiichi
        | _dcRiichi, hand^.handDiscards == [] = handRiichi .~ DoubleRiichi
        | _dcRiichi                           = handRiichi .~ Riichi
        | otherwise                           = id
    setIppatsu = handIppatsu .~ if _dcRiichi then True else False

-- | Automatically execute a discard necessary to advance the game (in case
-- of inactive players).
handAutoDiscard :: CanError m => HandA -> m Discard
handAutoDiscard hand
    | p : _ <- _handPicks hand = return $ Discard (pickedTile p) Nothing False
    | otherwise                = return $ Discard (hand ^?! handConcealed._Wrapped._last) Nothing False

-- * Winning

-- | Shout is Nothing if Tsumo
handWin :: CanError m => Maybe Shout -> HandA -> m HandA
handWin ms h
    | not (complete h)                                 = throwError "Cannot win with an incomplete hand"
    | isJust ms, h^.handFuriten._Wrapped /= NotFuriten = throwError "You are furiten"
    | otherwise                                        = return $ setAgari ms h

-- * Kan

-- | Ankan on the given tile if possible.
--
-- Remember to flip dora when this succeeds.
ankanOn :: CanError m => Tile -> HandA -> m HandA
ankanOn tile hand
    | [_,_,_,_] <- sameConcealed  = return hand'
    | [_,_,_]   <- sameConcealed
    , tile `elem` map pickedTile (hand^.handPicks) = return $ hand' & handPicks %~ filter ((/= tile) . pickedTile)
    | otherwise                                    = throwError "Not enough same tiles"
    where
        sameConcealed = hand^.handConcealed._Wrapped^..folded.filtered (== tile)
        hand'         = hand & handConcealed._Wrapped %~ filter (/= tile)
                             & handCalled %~ (:) (kantsu tile)
                             & handState .~ DrawFromWanpai

-- | Shouminkan with the tile if possible.
--
-- Remember to flip dora when this succeeds.
shouminkanOn :: CanError m => Tile -> HandA -> m HandA
shouminkanOn tile hand = do
    hand' <- tile `tileFromHand` hand
    let isShoum m = mentsuKind m == Koutsu && mentsuTile m == tile
    case hand' ^? handCalled.each.filtered isShoum of
        Nothing -> throwError "Shouminkan not possible: no such open koutsu"
        Just _  -> return $ handCalled.each.filtered isShoum %~ promoteToKantsu $ hand'

-- * Call

-- | Meld the mentsu to the hand
meldTo :: CanError m => Shout -> Mentsu -> HandA -> m HandA
meldTo shout mentsu hand
    | hand^.handConcealed._Wrapped.to (\xs -> length ih + length (xs L.\\ ih) == length xs)
    = if' (shoutKind shout == Ron) (handWin $ Just shout) return
    $ if' (shoutKind shout == Kan) (handState .~ DrawFromWanpai) id
    $ handCalled %~ (|> mentsu)
    $ handConcealed._Wrapped %~ (L.\\ ih)
    $ hand
    | otherwise = throwError "meldTo: Tiles not available"
  where ih = shoutTo shout

-- | Transfer the discard from the hand to a mentsu specified by the shout.
shoutFromHand :: CanError m => Kaze -> Shout -> HandA -> m (Mentsu, HandA)
shoutFromHand sk shout hand =
    case hand ^? handDiscards._last of
        Nothing          -> throwError "Player hasn't discarded anything"
        Just Discard{..} -> do
            isJust _dcTo `when` throwError "The discard has already been claimed"
            (shoutTile shout /= _dcTile) `when` throwError "The discard is not the shouted tile"
            return (fromShout shout, hand & handDiscards._last.dcTo .~ Just sk)

-- * Valued hand

valueHand :: Kaze -> HandA -> Kyoku -> ValuedHand
valueHand player h deal = ValuedHand (h^.handCalled) (h^.handConcealed._Wrapped) (getValue vi)
  where vi = ValueInfo deal player h

-- * Utility

-- | All mentsu that could be melded with hand given some tile.
shoutsOn :: Kaze -- ^ Shout from (player in turn)
         -> Tile -- ^ Tile to shout
         -> Kaze -- ^ Shouter
         -> HandA -- ^ shouter's
         -> [Shout]
shoutsOn np t p hand
    | np == p   = [] -- You're not shouting the thing you just discarded from yourself, right?
    | otherwise = concatMap toShout $ possibleShouts t
  where
    normalShuntsu     = nextKaze np == p
    ih                = sort (runIdentity $ _handConcealed hand) -- NOTE: sort
    toShout (mk, xs)  = do
        guard $ xs `isSubListOf` ih
        s <- case mk of
                Jantou  -> [Ron]
                Kantsu  -> [Kan]
                Koutsu  -> [Pon, Ron]
                Shuntsu -> [Chi, Ron]
        when (s == Chi) $ guard normalShuntsu
        when (hand^.handRiichi /= NoRiichi) $ guard (s == Ron)
        guard $ if s == Ron
            then complete ( toMentsu mk t xs : (hand^.handCalled), hand^.handConcealed._Wrapped L.\\ xs)
            else mk /= Jantou
        return $ Shout s np t xs

-- | Tiles the hand can discard for a riichi.
handCanRiichiWith :: HandA -> [Tile]
handCanRiichiWith h
    | h^.handRiichi /= NoRiichi = []
    | otherwise                 = h^.handConcealed._Wrapped.to (mapMaybe f)
    where f t = guard (canRiichiWith t h) >> return t

canRiichiWith :: Tile -> HandA -> Bool
canRiichiWith t h = null (h^.handCalled) && tenpai (L.delete t tiles)
    where tiles = h^.handConcealed._Wrapped ++ map pickedTile (h^.handPicks)

furiten :: HandA -> Bool
furiten h = any (`elem` (h^..handDiscards.each.dcTile)) . concatMap getAgari
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

-- | Set PickedTile from a agari call
setAgari :: Maybe Shout -> HandA -> HandA
setAgari ms h = h & handPicks .~ agari
    where agari | Just sh <- ms = [AgariCall (shoutTile sh) (shoutFrom sh)]
                | otherwise     = h^.handPicks & _last %~ AgariTsumo . pickedTile

-- | Take the tile from hand if possible
tileFromHand :: CanError m => Tile -> HandA -> m HandA
tileFromHand tile hand
    | pick : _ <- filter ((== tile).pickedTile) (hand^.handPicks) = return $ handPicks %~ L.delete pick $ hand
    | (xs, _ : ys) <- break (== tile) (runIdentity $ _handConcealed hand) = return $ handConcealed._Wrapped .~ (xs ++ ys) $ hand
    | otherwise                                                   = throwError "Tile not in hand"

-- |
-- >>> [1..5] `isSubListOf` [1..9]
-- True
--
-- >>> [1,1,1] `isSubListOf` [1,2,1,1]
-- True
--
-- >>> [1,1] `isSubListOf` [1, 2, 3]
-- False
isSubListOf :: Eq a => [a] -> [a] -> Bool
isSubListOf xs ys = length ys - length (ys L.\\ xs) == length xs
