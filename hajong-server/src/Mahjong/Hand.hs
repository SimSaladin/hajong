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
    , Hand(..), HandA, HandP, Discard(..), RiichiState(..), DrawState(..),
    , PickedTile(..), FuritenState(..), HandFlag(..),

    -- ** lenses
    , handCalled   
    , handDiscards 
    , handRiichi   
    , handIppatsu  
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
        maskPickedTile (FromWall _)         = FromWall Nothing
        maskPickedTile (FromWanpai _)       = FromWanpai Nothing
        maskPickedTile (AgariTsumo t)       = AgariTsumo t
        maskPickedTile (AgariCall s)        = AgariCall s
        maskPickedTile (AgariTsumoWanpai t) = AgariTsumoWanpai t

convertHand :: HandA -> HandP
convertHand hand = hand { _handPicks     = map convertPickedTile (_handPicks hand)
                        , _handConcealed = Just . runIdentity $ _handConcealed hand
                        , _handFuriten   = Just . runIdentity $ _handFuriten hand
                        , _handCanTsumo  = Just . runIdentity $ _handCanTsumo hand
                        , _handFlags     = Just . runIdentity $ _handFlags hand }
    where
        convertPickedTile (FromWall t)         = FromWall (Just $ runIdentity t)
        convertPickedTile (FromWanpai t)       = FromWanpai (Just $ runIdentity t)
        convertPickedTile (AgariTsumo t)       = AgariTsumo t
        convertPickedTile (AgariCall s)        = AgariCall s
        convertPickedTile (AgariTsumoWanpai t) = AgariTsumoWanpai t

valueHand :: Kaze -> HandA -> Kyoku -> ValuedHand
valueHand player h deal = ValuedHand (h^.handCalled) (h^.handConcealed._Wrapped) (getValue vi)
  where vi = ValueInfo deal player h

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
    return $ updateAfterPick (FromWall $ pure t) h

toHandWanpai :: CanError m => Tile -> HandA -> m HandA
toHandWanpai t h = do
    unless (h^.handState == DrawFromWanpai) $ throwError $ "Hand state was " <> tshow (h^.handState) <> ", but expected " <> tshow DrawFromWanpai
    return $ updateAfterPick (FromWanpai $ pure t) h

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

-- | The hand goes out with tsumo (ms=Nothing) or with a shout (ms=Just
-- shout).
handWin :: CanError m => Maybe Shout -> HandA -> m HandA
handWin ms h
    | isJust ms, h^.handFuriten._Wrapped /= NotFuriten = throwError "You are furiten"
    -- Just s <- ms, [] <- shoutTo s                    = return $ setAgari ms h -- XXX: for kokushi tenpai
    | not $ complete $ setAgari ms h                                 = throwError $ "Cannot win with an incomplete hand: " ++ tshow (ms, h)
    | otherwise                                        = return $ setAgari ms h

-- | Ankan on the given tile if possible.
--
-- Remember to flip dora when this succeeds.
ankanOn :: CanError m => Tile -> HandA -> m HandA
ankanOn tile hand
    | sameConcealed >= 4 = return hand'
    | sameConcealed == 3, tile `elem` map pickedTile (hand^.handPicks)
                         = return $ hand' & handPicks %~ L.deleteBy (\a b -> pickedTile a ==~ pickedTile b) (FromWall $ return tile) -- TODO a bit of a hack; looks better if PickedTile -> Tile and agari to its own field
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
    . set (handCanTsumo._Wrapped) False                  -- tsumo possible at least after discard
    . over handDiscards (|> d)                           -- Discard to discard pool
    $ movePicks hand                                     -- handPicks -> handConcealed
  where
    setRiichi
        | _dcRiichi, null (hand^.handDiscards) = handRiichi .~ DoubleRiichi
        | _dcRiichi                            = handRiichi .~ Riichi
        | otherwise                            = id
    updateFlags                                = handFlags._Wrapped %~ deleteSet HandFirsRoundUninterrupted
    movePicks                                  = over (handConcealed._Wrapped) (++ map pickedTile (_handPicks hand)) . set handPicks []
    updateFuriten                              = handFuriten._Wrapped .~ if' (furiten hand) Furiten NotFuriten

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
meldTo :: CanError m => Shout -> Mentsu -> HandA -> m HandA
meldTo shout mentsu hand -- TODO why must the shout be passed separetely?
    | correctConcealedTilesInHand = if' (shoutKind shout `elem` [Ron, Chankan]) (handWin $ Just shout) return
                                  $ if' (shoutKind shout == Kan) (handState .~ DrawFromWanpai) id $ moveFromConcealed hand
    | otherwise                   = throwError $ "meldTo: Tiles not available (concealed: " ++ tshow concealedTiles ++ ", needed: " ++ tshow tilesFromHand ++ ")"
  where
    tilesFromHand               = shoutTo shout
    concealedTiles              = hand^.handConcealed._Wrapped
    correctConcealedTilesInHand = length concealedTiles == length (concealedTiles L.\\ tilesFromHand) + length tilesFromHand
    moveFromConcealed           = over handCalled (|> mentsu) . over (handConcealed._Wrapped) removeTiles
    removeTiles ih              = let (aka, notaka) = partition isAka ih in (aka ++ notaka) L.\\ tilesFromHand -- XXX: this is needed because Eq on Tile is warped

-- XXX: This should return only a HandA
shoutFromHand :: CanError m => Kaze -> Shout -> HandA -> m (Mentsu, HandA)
shoutFromHand sk shout hand
    | shoutKind shout == Chankan                     = return (fromShout shout, hand)
    | Just Discard{..} <- hand ^? handDiscards._last = return (fromShout shout, hand & handDiscards._last.dcTo .~ Just sk)
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
    toShout (mk, xs)  = do

        -- Tiles to shout with must be in our hand. *Uses semantic Eq*
        guard $ xs `isSubListOf` ih

        -- Set aka information to the shoutTo tiles, for as many as
        -- possible.
        let goAka akas (t:ts) | t `elem` akas = setAka t : goAka (L.delete t akas) ts
                              | otherwise     =        t : goAka             akas  ts
            goAka _        []                 = []
            tiles = goAka akaIh xs

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

-- | Set PickedTile from a agari call
setAgari :: Maybe Shout -> HandA -> HandA
setAgari ms h = h & handPicks %~ agari
    where agari | Just sh <- ms = (`snoc` AgariCall sh)
                | otherwise     = _last %~ (\case
                                           FromWanpai (Identity t) -> AgariTsumoWanpai t
                                           x                       -> AgariTsumo $ pickedTile x )

-- XXX: Could cache the result in the Hand data
handGetAgari :: HandA -> [Tile]
handGetAgari = L.nub . concatMap getAgari . tenpaiGroupings

-- | Take the tile from hand if possible
tileFromHand :: CanError m => Tile -> HandA -> m HandA
tileFromHand tile hand
    | pick : _ <- filter ((==~ tile).pickedTile) (hand^.handPicks) = return $ handPicks %~ L.deleteBy ((==~) `on` pickedTile) pick $ hand
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
