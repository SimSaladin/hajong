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
import Text.PrettyPrint.ANSI.Leijen (Pretty(..), string)
import Data.Maybe (fromJust)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import Mahjong.Hand.Mentsu
import Mahjong.Hand.Value
import Mahjong.Hand.Algo
import Mahjong.Hand.Yaku
import Mahjong.Tiles

-- * Hand

data HandPublic = HandPublic
                { _handCalled :: [Mentsu]
                , _handDiscards :: [(Tile, Maybe Kaze)]
                , _handRiichi :: Bool
                , _handDrawWanpai :: Bool -- ^ Should draw from wanpai
                , _handAgari :: Maybe Tile
                } deriving (Show, Read, Eq)

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handPick :: Maybe Tile
          , _handFuriten :: Maybe Bool -- ^ Just (temporary?)
          , _handPublic :: HandPublic
          , _hCanTsumo :: Bool
          } deriving (Show, Read, Eq)

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> Hand
initHand tiles = Hand tiles Nothing Nothing (HandPublic [] [] False False Nothing) False

-- ** Lenses

--
makeLenses ''HandPublic
makeLenses ''Hand

-- Instances

instance HasGroupings Hand where
    getGroupings h = getGroupings $ (,) <$> _handCalled . _handPublic <*> _handConcealed $ h

instance Pretty Hand where
    pretty h =
        prettyList' (h^.handConcealed) P.<+>
        maybe "" (("|-" P.<+>) . pretty) (h^.handPick)

instance Pretty HandPublic where
    pretty = do
        -- FIXME
        tilenum <- view (handCalled.to length) <&> (13 -) . (*3)
        return $ string $ unwords $ replicate tilenum "_"

-- * Draw

toHand :: CanError m => Tile -> Hand -> m Hand
toHand t = do
    h <- handPick .~ Just t
    return $ return $ if' (complete h) (hCanTsumo .~ True) id $ h

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
discard :: CanError m => Tile -> Hand -> m Hand
discard tile hand
    | hand^.handPublic.handDrawWanpai || canDraw hand
      = throwError "You need to draw first"
    | hand^.handPick == Just tile || not (hand^.handPublic.handRiichi)
      = movePick . setDiscard <$> tileFromHand tile hand
    | otherwise = throwError "Cannot change wait in riichi"
  where
    setDiscard = handPublic.handDiscards %~ (++ [(tile, Nothing)])
    movePick h
        | Just p <- _handPick h = h & (handConcealed %~ (|> p)) . (handPick .~ Nothing)
        | otherwise             = h

-- | Do riichi if possible and discard.
discardRiichi :: CanError m => Tile -> Hand -> m Hand
discardRiichi tile hand
    | hand ^. handPublic.handRiichi = throwError "Already in riichi"
    | canRiichiWith tile hand       = discard tile hand <&> handPublic.handRiichi .~ True
    | otherwise                     = throwError "Not in tenpai"

-- | Automatically execute a discard necessary to advance the game (in case
-- of inactive players).
handAutoDiscard :: CanError m => Hand -> m Tile
handAutoDiscard hand
    | Just tile <- _handPick hand = return tile
    | otherwise                   = return $ hand ^?! handConcealed._last

-- * Checks

-- | All mentsu that could be melded with hand given some tile.
shoutsOn :: Kaze -- ^ Shout from (player in turn)
         -> Tile -- ^ Tile to shout
         -> Kaze -- ^ Shouter
         -> Hand -- ^ shouter's
         -> [Shout]
shoutsOn np t p hand
    | np == p   = [] -- You're not shouting the thing you just discarded from yourself, right?
    | otherwise = concatMap toShout $ filter (\xs -> snd xs `isInfixOf` ih) $ possibleShouts (nextKaze np == p) t
  where
    ih = sort (_handConcealed hand) -- NOTE: sort
    toShout (mk, xs) = do
        guard $ xs `isInfixOf` ih
        s <- case mk of
                Jantou  -> [Ron]
                Kantsu  -> [Kan]
                Koutsu  -> [Pon, Ron]
                Shuntsu -> [Chi, Ron]
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

handWin :: CanError m => Hand -> m Hand
handWin h = if complete h then return h else throwError "Cannot tsumo, hand is not complete"

-- | Tiles the hand can discard for a riichi.
handCanRiichiWith :: Hand -> [Tile]
handCanRiichiWith h = mapMaybe (\t -> return t <* guard (canRiichiWith t h)) (h^.handConcealed)

canRiichiWith :: Tile -> Hand -> Bool
canRiichiWith t h = tenpai
    (h^.handPublic.handCalled, L.delete t $ h^.handConcealed ++ maybe [] return (h^.handPick))

-- * Kan

-- | Do an ankan on the given tile.
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

shouminkanOn :: CanError m => Tile -> Hand -> m Hand
shouminkanOn tile hand = do
    hand' <- tile `tileFromHand` hand
    case hand' ^? handPublic.handCalled.each.filtered isk of
        Just _  -> return $ hand & handPublic.handCalled.each.filtered isk %~ promoteToKantsu
        Nothing -> throwError "shouminkan not possible: no such open koutsu"
  where
    isk m = mentsuKind m == Koutsu && mentsuTile m == tile

-- | Take the tile from hand if possible
tileFromHand :: CanError m => Tile -> Hand -> m Hand
tileFromHand tile hand
    | Just tile' <- hand ^. handPick, tile == tile'         = return $ handPick .~ Nothing $ hand
    | (xs, _ : ys) <- break (== tile) (_handConcealed hand) = return $ handConcealed .~ (xs ++ ys) $ hand
    | otherwise                                             = throwError "Tile not in hand"

-- * Call

-- | Meld the mentsu to the hand
meldTo :: CanError m => Shout -> Mentsu -> Hand -> m Hand
meldTo shout mentsu hand
    | hand^.handConcealed.to (\xs -> length ih + length (xs L.\\ ih) == length xs)
    = return $ handPublic.handCalled %~ (|> mentsu)
             $ handConcealed %~ (L.\\ ih)
             $ if' (shoutKind shout == Kan) (handPublic.handDrawWanpai .~ True) id
             $ hand
    | otherwise = throwError "meldTo: Tiles not available"
  where
    ih = shoutTo shout

-- | Transfer the discard from the hand to a mentsu specified by the shout.
shoutFromHand :: CanError m => Kaze -> Shout -> Hand -> m (Mentsu, Hand)
shoutFromHand sk shout hand =
    case hand ^? handPublic.handDiscards._last of
        Nothing          -> throwError "Player hasn't discarded anything"
        Just (_, Just _) -> throwError "The discard has already been claimed"
        Just (t, _)
            | shoutTile shout /= t -> throwError "The discard is not the shouted tile"
            | otherwise              -> return
                (fromShout shout, hand & handPublic.handDiscards._last .~ (t, Just sk))

-- * Valued hand

-- | A hand that won.
data ValuedHand = ValuedHand
    { _vhMentsu :: [Mentsu]
    , _vhTiles  :: [Tile]
    , _vhValue  :: Value
    } deriving (Show, Read)

makeLenses ''ValuedHand

valueHand :: Hand -> Kaze -> Kaze -> ValuedHand
valueHand h r player = ValuedHand (h^.handPublic.handCalled) (h^.handConcealed) (getValue vi)
  where vi = ValueInfo r player
            (h^.handPublic.handRiichi)
            (h^.handPublic.handCalled.to null)
            (h^.handPublic.handDiscards^..each._1)
            (h^.handPublic.handCalled)
            (h^.handPublic.handAgari.to fromJust)
