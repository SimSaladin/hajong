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
module Mahjong.Hand where

import qualified Data.List as L
import Text.PrettyPrint.ANSI.Leijen (Pretty(..), string)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import Mahjong.Hand.Mentsu
import Mahjong.Hand.Algo
import Mahjong.Tiles

-- * Hand

data HandPublic = HandPublic
                { _handOpen :: [Mentsu]
                , _handDiscards :: [(Tile, Maybe Kaze)]
                , _handRiichi :: Bool
                , _handDrawWanpai :: Bool -- ^ Should draw from wanpai
                } deriving (Show, Read, Eq)

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handPick :: Maybe Tile
          , _handFuriten :: Maybe Bool -- ^ Just (temporary?)
          , _handPublic :: HandPublic
          } deriving (Show, Read, Eq)

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> Hand
initHand tiles = Hand tiles Nothing Nothing (HandPublic [] [] False False)

-- ** Lenses

--
makeLenses ''HandPublic
makeLenses ''Hand

instance HasGroupings Hand where
    getGroupings h = getGroupings $ (,) <$> _handOpen . _handPublic <*> _handConcealed $ h

instance Pretty Hand where
    pretty h =
        prettyList' (h^.handConcealed) P.<+>
        maybe "" (("|-" P.<+>) . pretty) (h^.handPick)

instance Pretty HandPublic where
    pretty = do
        -- FIXME
        tilenum <- view (handOpen.to length) <&> (13 -) . (*3)
        return $ string $ unwords $ replicate tilenum "_"

-- * Discard

-- | Discard a tile; fails if
--
--  1. tile not in the hand
--  2. riichi restriction
--  3. need to draw first
discard :: CanError m => Tile -> Hand -> m Hand
discard tile hand
    | hand ^. handPick == Just tile = return $ hand & set handPick Nothing . setDiscard
    | hand ^. handPublic.handRiichi = throwError "Cannot change wait in riichi"
    | otherwise = case ys of
        [] -> throwError "Tile not in hand"
        _ : ys' | hand^.handPublic.handDrawWanpai || canDraw hand -> throwError "You need to draw first"
                | Just pick <- hand^.handPick -> return
                    $ handPick .~ Nothing $ handConcealed .~ (pick : xs ++ ys') $ setDiscard hand
                | otherwise -> return $ handConcealed .~ (xs ++ ys') $ setDiscard hand
    where
        (xs, ys)   = break (== tile) (_handConcealed hand)
        setDiscard = handPublic.handDiscards %~ (++ [(tile, Nothing)])

-- | Do riichi if possible and discard.
discardRiichi :: CanError m => Tile -> Hand -> m Hand
discardRiichi tile hand
    | hand ^. handPublic.handRiichi = throwError "Already in riichi"
    | tenpai hand                   = discard tile $ handPublic.handRiichi.~True $ hand
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
                then complete ( toMentsu mk t xs : (hand^.handPublic.handOpen)
                              , _handConcealed hand L.\\ xs )
                else mk /= Jantou
        return $ Shout s np t xs

-- | From wall (not wanpai).
canDraw :: Hand -> Bool
canDraw h = not (h^.handPublic.handDrawWanpai)
    && isNothing (h^.handPick)
    && (3 * length (h^.handPublic.handOpen) + length (h^.handConcealed) == 13)

-- * Ankan

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
                             & handPublic.handOpen %~ (:) (kantsu tile)
                             & handPublic.handDrawWanpai .~ True

-- * Call

-- | Meld the mentsu to the hand
meldTo :: CanError m => Shout -> Mentsu -> Hand -> m Hand
meldTo shout mentsu hand
    | hand^.handConcealed.to (\xs -> length ih + length (xs L.\\ ih) == length xs)
    = return $ handPublic.handOpen %~ (|> mentsu)
             $ handConcealed %~ (L.\\ ih)
             $ hand
    | otherwise = throwError "meldTo: Tiles not available"
  where
    ih = shoutedTo shout

-- | Transfer the discard from the hand to a mentsu specified by the shout.
shoutFromHand :: CanError m => Kaze -> Shout -> Hand -> m (Mentsu, Hand)
shoutFromHand sk shout hand =
    case hand ^? handPublic.handDiscards._last of
        Nothing          -> throwError "Player hasn't discarded anything"
        Just (_, Just _) -> throwError "The discard has already been claimed"
        Just (t, _)
            | shoutedTile shout /= t -> throwError "The discard is not the shouted tile"
            | otherwise              -> return
                (fromShout shout, hand & handPublic.handDiscards._last .~ (t, Just sk))

-- * Hand value

-- | Required info to calculate the value from a hand.
data ValueInfo = ValueInfo
              { vRound :: Kaze
              , vPlayer :: Kaze
              , vRiichi :: Bool
              , vConcealed :: Bool
              , vDiscarded :: [Tile] -- ^ To check furiten
              , vMentsu :: [Mentsu]
              , vWinWith :: Tile
              } deriving (Show)

data Value = Value [Yaku] Fu
type Fu    = Int

-- ** Yaku Information

data Yaku = Yaku
          { yaku :: Int
          , yakuName :: Text
          }
