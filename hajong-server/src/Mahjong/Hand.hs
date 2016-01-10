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

    -- * Hand sub-modules
    , module Mahjong.Hand.Algo
    , module Mahjong.Hand.Mentsu
    , module Mahjong.Hand.Value
    , module Mahjong.Hand.Yaku

    -- * Types and lenses
    , Hand(..), HandA, HandP, Discard(..), RiichiState(..), DrawState(..)
    , PickedTile(..), FuritenState(..), HandFlag(..), Agari(..)

    -- ** lenses
    , handCalled
    , handDiscards
    , handRiichi
    , handIppatsu
    , handAgari
    , handState
    , handPicks
    , handConcealed
    , handFuriten
    , handCanTsumo
    , handFlags
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

-- * Functions

-- | Automatically execute a discard necessary to advance the game (in case
-- of inactive players).
handAutoDiscard :: CanError m => HandA -> m Discard
handAutoDiscard hand
    | p : _ <- _handPicks hand = return $ Discard (pickedTile p) Nothing False
    | otherwise                = return $ Discard (hand ^?! handConcealed._Wrapped._last) Nothing False

-- | Hide private info from the data type.
maskPublicHand :: HandA -> HandP
maskPublicHand hand =
    hand { _handPicks = map maskPickedTile (_handPicks hand)
         , _handConcealed = Nothing
         , _handFuriten = Nothing
         , _handCanTsumo = Nothing
         , _handFlags    = Just $ runIdentity $ _handFlags hand }
    where
        maskPickedTile (PickedTile _ wanpai) = PickedTile Nothing wanpai

-- | Doesn't discard information
convertHand :: HandA -> HandP
convertHand hand = hand { _handPicks     = map convertPickedTile (_handPicks hand)
                        , _handConcealed = Just . runIdentity $ _handConcealed hand
                        , _handFuriten   = Just . runIdentity $ _handFuriten hand
                        , _handCanTsumo  = Just . runIdentity $ _handCanTsumo hand
                        , _handFlags     = Just . runIdentity $ _handFlags hand }
    where
        convertPickedTile (PickedTile (Identity t) wanpai) = PickedTile (Just t) wanpai

valueHand :: Kaze -> HandA -> Kyoku -> ValuedHand
valueHand player h deal = ValuedHand called concealed (getValue vi)
  where
    vi = ValueInfo deal player h
    (called, concealed) = case h^.handAgari of
                              Nothing -> (h^.handCalled, h^.handConcealed._Wrapped)
                              Just (AgariTsumo tsumo _) -> (h^.handCalled, h^.handConcealed._Wrapped ++ [tsumo])
                              Just (AgariCall call) -> (h^.handCalled ++ [fromShout call], h^.handConcealed._Wrapped)

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

handInNagashi :: HandA -> Bool
handInNagashi h = all id [ h^.handCalled == []
                         , null $ h^..handDiscards.traversed.filtered (isJust . _dcTo)
                         , null $ h^..handDiscards.traversed.filtered (isSuited . _dcTile) ]

-- * Player actions

toHand :: CanError m => Tile -> HandA -> m HandA
toHand t h = do
    unless (h^.handState == DrawFromWall) $ throwError $ "Hand state was " <> tshow (h^.handState) <> ", but expected " <> tshow DrawFromWall
    return $ updateAfterPick (PickedTile (pure t) False) h

toHandWanpai :: CanError m => Tile -> HandA -> m HandA
toHandWanpai t h = do
    unless (h^.handState == DrawFromWanpai) $ throwError $ "Hand state was " <> tshow (h^.handState) <> ", but expected " <> tshow DrawFromWanpai
    return $ updateAfterPick (PickedTile (pure t) True) h

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

    | hand^.handRiichi /= NoRiichi, p : _ <- hand^.handPicks, pickedTile p /= _dcTile
                                                  = throwError "Cannot change wait in riichi"
    | otherwise                                   = updateAfterDiscard d <$> tileFromHand _dcTile hand

-- | The tiles of the shout (including shoutTo-tiles) must NOT be present
-- in the hand when this function is called.
rons :: CanError m => Shout -> HandA -> m HandA
rons shout hand
    | hand^.handFuriten._Wrapped /= NotFuriten = throwError "You are furiten"
    -- | [] <- shoutTo shout                      = return $ setAgariCall shout hand -- XXX: for kokushi tenpai
    | not $ complete $ setAgariCall shout hand = throwError $ "Cannot win with an incomplete hand: " ++ tshow (shout, hand)
    | otherwise                                = return $ setAgariCall shout hand

-- | Ankan on the given tile if possible.
--
-- Remember to flip dora when this succeeds.
ankanOn :: CanError m => Tile -> HandA -> m HandA
ankanOn tile hand
    | sameConcealed >= 4 = return hand'
    | sameConcealed == 3, tile `elem` map pickedTile (hand^.handPicks)
                         = return $ handPicks %~ L.deleteBy (on (==~) pickedTile) (PickedTile (pure tile) False) $ hand'
    | otherwise          = throwError "Not enough same tiles"
  where
    sameConcealed = hand^.handConcealed._Wrapped^..folded.filtered (==~ tile) & length
    hand'         = hand & handConcealed._Wrapped %~ L.foldl1' (.) (replicate 4 (L.delete tile)) -- @delete@, not @filter@: allows for hypotethical situation of more than 4 of the same tile
                         & handCalled %~ (:) (kantsu tile)
                         & handState .~ DrawFromWanpai

-- | Shouminkan with the tile if possible.
--
-- Remember to flip dora when this succeeds.
shouminkanOn :: CanError m => Tile -> HandA -> m HandA
shouminkanOn tile hand = do
    hand' <- tile `tileFromHand` hand
    let isShoum m = mentsuKind m == Koutsu && headEx (mentsuTiles m) ==~ tile
    case hand' ^? handCalled.each.filtered isShoum of
        Nothing -> throwError "Shouminkan not possible: no such open koutsu"
        Just _  -> return $ handCalled.each.filtered isShoum %~ promoteToKantsu
                          $ handState .~ DrawFromWanpai
                          $ hand'

-- ** State changes from actions

updateAfterDiscard :: Discard -> HandA -> HandA
updateAfterDiscard d@Discard{..} hand = updateFlags
    . updateFuriten                                      -- If the discard brought us to furiten state set a flag -- XXX: should be a flag
    . setRiichi                                          -- If we riichi with the discard, set a flag -- XXX: should be a flag
    . set handIppatsu (if' _dcRiichi True False)         -- Ippatsu-flag is set here XXX: should be a flag
    . set (handCanTsumo._Wrapped) False                  -- tsumo impossible at least after discard
    . over handDiscards (|> d)                           -- Discard to discard pool
    $ movePicks hand                                     -- handPicks -> handConcealed
  where
    setRiichi
        | _dcRiichi, null (hand^.handDiscards) = handRiichi .~ DoubleRiichi
        | _dcRiichi                            = handRiichi .~ Riichi
        | otherwise                            = id
    updateFlags                                = handFlags._Wrapped %~ deleteSet HandFirsRoundUninterrupted
    movePicks                                  = over (handConcealed._Wrapped) (++ map pickedTile (_handPicks hand)) . set handPicks []
    updateFuriten h                            = h & handFuriten._Wrapped .~ if' (furiten h) Furiten NotFuriten

-- | Update hand state after picked tile
updateAfterPick :: PickedTile Identity -> HandA -> HandA
updateAfterPick pick h = if' (complete h') (handCanTsumo .~ pure True) id h'
  where
     h' = handPicks %~ (++ [pick]) $ handState.~DrawNone $ h

-- * Calling

-- | Meld the mentsu to the hand. on win sets the agari info
--
-- * if the hand wins, call handWin to set agari tile.
-- * move the melded mentsu to called.
-- * if the shout was kan, meld it and set state to DrawFromWanpai.
meldTo :: CanError m => Shout -> HandA -> m HandA
meldTo shout hand
    | not correctConcealedTilesInHand       = throwError $ "meldTo: Tiles not available (concealed: " ++ tshow concealedTiles ++ ", needed: " ++ tshow tilesFromHand ++ ")"
    | shoutKind shout `elem` [Ron, Chankan] = rons shout (removeFromConcealed hand)
    | shoutKind shout == Kan                = return $ (handState .~ DrawFromWanpai) (moveFromConcealed hand)
    | otherwise                             = return $ moveFromConcealed hand
  where
    tilesFromHand               = shoutTo shout
    concealedTiles              = hand^.handConcealed._Wrapped
    correctConcealedTilesInHand = length concealedTiles == length (concealedTiles L.\\ tilesFromHand) + length tilesFromHand
    moveFromConcealed           = over handCalled (|> fromShout shout) . removeFromConcealed
    removeFromConcealed         = over (handConcealed._Wrapped) removeTiles
    removeTiles ih              = let (aka, notaka) = partition isAka ih in (aka ++ notaka) L.\\ tilesFromHand -- XXX: this is needed because Eq on Tile is warped

shoutFromHand :: CanError m => Kaze -> Shout -> HandA -> m HandA
shoutFromHand sk shout hand
    | shoutKind shout == Chankan                     = return hand
    | Just Discard{..} <- hand ^? handDiscards._last = return $ handDiscards._last.dcTo .~ Just sk $ hand
    | otherwise                                      = throwError "shoutFromHand: There were no discards on the hand."

-- | All mentsu that could be melded with hand given some tile.
shoutsOn :: Kaze -- ^ Shout from (player in turn)
         -> Tile -- ^ Tile to shout
         -> Kaze -- ^ Shouter
         -> HandA -- ^ shouter's
         -> [Shout]
shoutsOn np t p hand
    | np == p                   = [] -- You're not shouting the thing you just discarded from yourself, right?
    | Just x <- kokushiAgari (hand^.handConcealed._Wrapped)
    , either (==~ t) (elem t) x = [Shout Ron np t []] -- Ron kokushi
    | otherwise                 = concatMap toShout $ possibleShouts t
  where
    canChi            = succCirc np == p
    ih                = sort (runIdentity $ _handConcealed hand) -- NOTE: sort
    akaIh             = filter isAka ih
    toShout (mk, st)  = do

        -- Tiles to shout with must be in our hand. *Uses semantic Eq*
        guard $ st `isSubListOf` ih

        -- Set aka information to the shoutTo tiles, for as many as
        -- possible.
        let goAka akas (x:xs) | x `elem` akas = setAka x : goAka (L.delete x akas) xs
                              | otherwise     =        x : goAka             akas  xs
            goAka _        []                 = []
            tiles = goAka akaIh st

        s <- case mk of
                Jantou  -> [Ron]
                Kantsu  -> [Kan]
                Koutsu  -> [Pon, Ron]
                Shuntsu -> [Chi, Ron]

        -- require Chi is from previous player
        when (s == Chi) $ guard canChi

        -- if riichi, only winning shouts. NOTE: chankan is handled after
        -- the shoutsOn function.
        when (hand^.handRiichi /= NoRiichi) $ guard (s == Ron)

        when (s == Ron) $ guard $ complete
            -- XXX: This looks fragile...
            (toMentsu mk t tiles : (hand^.handCalled), hand^.handConcealed._Wrapped L.\\ tiles)

        return $ Shout s np t tiles

-- * Utility

setAgariTsumo :: HandA -> HandA
setAgariTsumo hand = case hand^?handPicks._last of
    Just (PickedTile (Identity t) wanpai) -> hand & handPicks %~ initEx & handAgari .~ Just (AgariTsumo t wanpai)
    Nothing -> error "Can't tsumo with no picked tile"

setAgariCall :: Shout -> HandA -> HandA
setAgariCall shout = handAgari .~ Just (AgariCall shout)

-- XXX: Could cache the result in the Hand data
handGetAgari :: HandA -> [Tile]
handGetAgari = L.nub . concatMap getAgari . tenpaiGroupings

-- | Take the tile from hand if possible.
tileFromHand :: CanError m => Tile -> HandA -> m HandA
tileFromHand tile hand
    | pick : _ <- filter ((==@ tile).pickedTile) (hand^.handPicks) = return $ handPicks %~ L.deleteBy ((==@) `on` pickedTile) pick $ hand
    | (xs, _ : ys) <- break (==@ tile) (hand^.handConcealed._Wrapped) = return $ handConcealed._Wrapped .~ (xs ++ ys) $ hand
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
