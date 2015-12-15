{-# LANGUAGE DeriveFunctor #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Yaku.Builder
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Hand.Yaku.Builder
    ( YakuCheck, runYakuCheck

    -- * Checkers
    , concealedHandDegrade, concealedHand, openHand, yakuState, allMentsuOfKind, yakuFail

    -- * Matching mentsu

    -- ** Any
    , anyKoutsu, anyKantsu, anyShuntsu, anyJantou, anyMentsu
    , anyKoutsuKantsu, anyMentsuJantou

    -- *** Stateful
    , anyShuntsu', anyMentsu', anyKoutsuKantsu', anyMentsuJantou'

    -- ** Specific
    , terminal, honor, sangenpai, suited, anyTile, concealed
    , sameTile, containsTile, sameNumber, sameSuit, ofNumber

    -- ** Combinators
    , (&.), (|.), propNot, tileGroupHead, tileGroupTiles
    ) where

------------------------------------------------------------------------------
import           Import
import           Mahjong.Tiles (Tile(..), Number(..))
import qualified Mahjong.Tiles as T
import           Mahjong.Hand.Mentsu
import           Mahjong.Hand.Algo
------------------------------------------------------------------------------
import           Mahjong.Hand.Internal
import           Mahjong.Kyoku.Internal
------------------------------------------------------------------------------
import           Control.Monad.Free
import           Control.Monad.State
------------------------------------------------------------------------------

type YakuCheck = Free Check

data Check next = YakuMentsu MentsuProp next
                -- ^ Require a simple mentsu property. Requiring this
                -- first could allow for simple optimization by
                -- removing some repeated checking.

                | YakuMentsu' MentsuProp (TileGroup -> next)
                -- ^ Require a mentsu property, but allow upcoming
                -- properties depend on the matched TileGroup.

                | YakuStateful (ValueInfo -> next)
                -- ^ Depend on game state.

                | YakuHandConcealedDegrades next
                | YakuHandConcealed next
                | YakuHandOpen next
                | YakuFailed
                deriving (Functor)

-- | Run the yaku checker with the given grouping.
runYakuCheck :: ValueInfo -> Grouping -> YakuCheck Yaku -> Maybe Yaku
runYakuCheck info grouping = fmap fst . (`runStateT` grouping) . iterM f
    where
        f :: Check (StateT Grouping Maybe Yaku) -> StateT Grouping Maybe Yaku
        f (YakuMentsu  mp s)            = get >>= lift . findMatch mp >>= putRes >>  s
        f (YakuMentsu' mp s)            = get >>= lift . findMatch mp >>= putRes >>= s
        f (YakuStateful s)              = s info
        f (YakuHandConcealedDegrades s) = if isConcealed then s            else s <&> (yHan -~ 1)
        f (YakuHandConcealed s)         = if isConcealed then s            else lift Nothing
        f (YakuHandOpen s)              = if isConcealed then lift Nothing else s
        f YakuFailed                    = lift Nothing

        putRes (tg, g) = put g >> return tg
        isConcealed    = null $ info^..vHand.handCalled.each.filtered (maybe True ((/= Ron) . shoutKind) . mentsuShout)

-- @MentsuProp@s

data MentsuProp = TileTerminal
                | TileSameAs Tile
                | TileContained Tile
                | TileSuited
                | TileSameSuit Tile
                | TileSameNumber Tile
                | TileNumber Number
                | TileHonor
                | TileSangenpai
                | TileAnd MentsuProp MentsuProp -- ^ &&
                | TileOr MentsuProp MentsuProp -- ^ ||
                | TileNot MentsuProp -- ^ not
                | TileConcealed
                | MentsuJantou
                | MentsuOrJantou
                | MentsuShuntsu
                | MentsuKoutsu
                | MentsuKantsu
                | MentsuKoutsuKantsu
                | PropAny -- ^ Match anything

-- | Binary combinations of mentsu porperties.
(&.), (|.) :: MentsuProp -> MentsuProp -> MentsuProp
(&.) = TileAnd
(|.) = TileOr
infixl 1 &., |.

-- @Check@ primitives

-- | Value degrades by one if open.
concealedHandDegrade :: YakuCheck ()
concealedHandDegrade = liftF $ YakuHandConcealedDegrades ()

-- | Must be concealed
concealedHand :: YakuCheck ()
concealedHand = liftF $ YakuHandConcealed ()

-- | Must be open
openHand :: YakuCheck ()
openHand = liftF $ YakuHandOpen ()

-- | Yaku that depends on something else than the mentsu; see "ValueInfo"
-- for available properties.
yakuState :: YakuCheck ValueInfo
yakuState = liftF (YakuStateful id)

-- | Simple yaku helper to require some same property from the four mentsu
-- and the pair.
allMentsuOfKind :: MentsuProp -> YakuCheck ()
allMentsuOfKind tkind = do
    anyJantou tkind
    replicateM_ 4 $ anyMentsu tkind

-- | Fail the hand
yakuFail :: YakuCheck a
yakuFail = liftF YakuFailed -- (error "Not used"))

-- | Require any mentsu with a property.
anyKoutsu, anyKantsu, anyShuntsu, anyJantou, anyMentsu, anyKoutsuKantsu, anyMentsuJantou :: MentsuProp -> YakuCheck ()
anyMentsu        tkind = liftF $ YakuMentsu (propNot MentsuJantou &. tkind) ()
anyKoutsu        tkind = liftF $ YakuMentsu (MentsuKoutsu         &. tkind) ()
anyShuntsu       tkind = liftF $ YakuMentsu (MentsuShuntsu        &. tkind) ()
anyKantsu        tkind = liftF $ YakuMentsu (MentsuKantsu         &. tkind) ()
anyJantou        tkind = liftF $ YakuMentsu (MentsuJantou         &. tkind) ()
anyKoutsuKantsu  tkind = liftF $ YakuMentsu (MentsuKoutsuKantsu   &. tkind) ()
anyMentsuJantou  tkind = liftF $ YakuMentsu (MentsuOrJantou       &. tkind) ()

-- | Require any mentsu with a property. Rest of the definition may depend
-- on the matched tile.
anyShuntsu', anyKoutsuKantsu', anyMentsu', anyMentsuJantou' :: MentsuProp -> YakuCheck TileGroup
anyMentsu'       tkind = liftF $ YakuMentsu' tkind id
anyKoutsuKantsu' tkind = liftF $ YakuMentsu' (MentsuKoutsuKantsu &. tkind) id
anyShuntsu'      tkind = liftF $ YakuMentsu' (MentsuShuntsu      &. tkind) id
anyMentsuJantou' tkind = liftF $ YakuMentsu' (MentsuOrJantou     &. tkind) id

tileGroupHead :: TileGroup -> Tile
tileGroupHead = headEx . tileGroupTiles

-- Mentsu properties

-- | Tile kinds.
terminal, honor, sangenpai, suited, anyTile, concealed :: MentsuProp
terminal  = TileTerminal
honor     = TileHonor
sangenpai = TileSangenpai
suited    = TileSuited
anyTile   = PropAny
concealed = TileConcealed

sameTile, containsTile, sameNumber, sameSuit :: Tile -> MentsuProp
sameTile = TileSameAs
containsTile = TileContained
sameNumber = TileSameNumber
sameSuit = TileSameSuit

ofNumber :: Number -> MentsuProp
ofNumber = TileNumber

-- | Negation of a MentsuProp.
propNot :: MentsuProp -> MentsuProp
propNot = TileNot

-- Check MentsuProps directly.

-- | Find a match in a list of mentsu. Returns the matches identifier tile and leftovers.
findMatch :: MentsuProp -> Grouping -> Maybe (TileGroup, Grouping)
findMatch _  []   = Nothing
findMatch mp (x:xs)
    | matchProp mp x = Just (x, xs)
    | otherwise      = findMatch mp xs & _Just._2 %~ (x:)

-- | Match a property on a TileGroup
matchProp :: MentsuProp -> TileGroup -> Bool
matchProp MentsuJantou       tg                     = isPair tg
matchProp MentsuOrJantou     _                      = True
matchProp MentsuShuntsu      (GroupComplete mentsu) = isShuntsu mentsu
matchProp MentsuKoutsu       (GroupComplete mentsu) = isKoutsu mentsu
matchProp MentsuKantsu       (GroupComplete mentsu) = isKantsu mentsu
matchProp MentsuKoutsuKantsu (GroupComplete mentsu) = isKantsu mentsu || isKoutsu mentsu
matchProp tt tg
    | TileTerminal        <- tt = any T.terminal tiles
    | TileSameAs tile     <- tt = headEx tiles == tile
    | TileContained tile  <- tt = tile `elem` tiles
    | TileSuited          <- tt = T.isSuited (headEx tiles)
    | TileSameSuit tile   <- tt = T.suitedSame tile (headEx tiles)
    | TileSameNumber tile <- tt = isJust (T.tileNumber tile) && T.tileNumber tile == T.tileNumber (headEx tiles)
    | TileNumber n        <- tt = T.tileNumber (headEx tiles) == Just n
    | TileHonor           <- tt = not $ T.isSuited (headEx tiles)
    | TileSangenpai       <- tt = T.sangenpai (headEx tiles)
    | TileAnd x y         <- tt = matchProp x tg && matchProp y tg
    | TileOr x y          <- tt = matchProp x tg || matchProp y tg
    | TileNot x           <- tt = not $ matchProp x tg
    | TileConcealed       <- tt = case tg of GroupComplete mentsu -> isNothing $ mentsuShout mentsu
                                             _ -> True
    | PropAny             <- tt = True
    | otherwise                 = False
  where
    tiles = tileGroupTiles tg
