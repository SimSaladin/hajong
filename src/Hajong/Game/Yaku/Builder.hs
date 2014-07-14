{-# LANGUAGE DeriveFunctor #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Yaku.Builder
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.Yaku.Builder where

import ClassyPrelude
import Control.Lens
import Control.Monad.Free
import Control.Monad.State
import qualified Data.List as L

import Hajong.Game.Mentsu
import Hajong.Game.Tiles

calculateYaku :: CompleteHand -> [Yaku Int] -- XXX: Well, maybe not so stupid return type
calculateYaku hand = undefined

runChecker :: YakuInfo -> CompleteHand -> Yaku Int -> Maybe Int
runChecker yi hand = fmap fst . (`runStateT` hand) . iterM f
    where
        f :: YakuChecker (StateT [Mentsu] Maybe Int) -> StateT [Mentsu] Maybe Int
        f (YakuMentsu  mp s)            = get >>= lift . findMatch mp >>= putRes >>  s
        f (YakuMentsu' mp f)            = get >>= lift . findMatch mp >>= putRes >>= f
        f (YakuStateful f)              = f yi
        f (YakuHandConcealedDegrades s) = if yakuIsConcealed yi then s else (\x -> x - 1) <$> s
        f (YakuHandConcealed s)         = if yakuIsConcealed yi then s else lift Nothing
        f (YakuHandOpen s)              = if yakuIsConcealed yi then lift Nothing else s

        putRes (xs, t) = put xs >> return t

-- | Find a match in a list of mentsu. Returns the matches identifier tile and leftovers.
findMatch :: MentsuProp -> [Mentsu] -> Maybe ([Mentsu], Tile)
findMatch _  []   = Nothing
findMatch mp (x:xs)
    | matchProp mp x = Just (xs, unsafeHead $ mentsuPai x)
    | otherwise      = (_1 %~ (x:)) <$> findMatch mp xs

-- | Match a property on a mentsu.
matchProp :: MentsuProp -> Mentsu -> Bool
matchProp tt mentsu
    | (first:_) <- mentsuPai mentsu = case tt of
        MentsuJantou       | isJantou mentsu       -> True
        MentsuAnyJantou    | not $ isJantou mentsu -> True
        MentsuShuntsu      | isShuntsu mentsu      -> True
        MentsuKoutsu       | isKoutsu mentsu       -> True
        MentsuKantsu       | isKantsu mentsu       -> True
        MentsuKoutsuKantsu | isKantsu mentsu || isKoutsu mentsu -> True
        -- XXX: this is incomplete (shuntsu + terminals etc.)
        TileTerminal        -> tileTerminal first
        TileSameAs tile     -> first == tile
        TileSuited          -> tileSuited first
        TileSameSuit tile   -> compareSuit tile first
        TileSameNumber tile -> tileNumber tile      == tileNumber first
        TileNumber n        -> tileNumber first == n
        TileHonor           -> not $ tileSuited first
        TileSangenpai       -> tileSangenpai first
        TileAnd x y         -> matchProp x mentsu && matchProp y mentsu
        TileOr x y          -> matchProp x mentsu || matchProp y mentsu
        TileNot x           -> not $ matchProp x mentsu
        TileConcealed       -> isNothing $ mentsuOpen mentsu
        PropAny             -> True
        _ -> True
    | otherwise = error "ofTileType: empty mentsu"

-- * Types

type Yaku = Free YakuChecker

data YakuChecker next = YakuMentsu MentsuProp next
                      -- ^ Require a simple mentsu property. Requiring this
                      -- first could allow for simple optimization by
                      -- removing some repeated checking.
                      | YakuMentsu' MentsuProp (Tile -> next)
                      -- ^ Require a mentsu property, but allow upcoming
                      -- properties depend on the matched tile.
                      | YakuStateful (YakuInfo -> next)
                      -- ^ Depend on game state.
                      | YakuHandConcealedDegrades next
                      | YakuHandConcealed next
                      | YakuHandOpen next
                      deriving (Functor)

data YakuInfo = YakuInfo
              { yakuRoundKaze :: Kazehai
              , yakuPlayerKaze :: Kazehai
              , yakuIsConcealed :: Bool
              }

data MentsuProp = TileTerminal
                | TileSameAs Tile
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
                | MentsuAnyJantou
                | MentsuShuntsu
                | MentsuKoutsu
                | MentsuKantsu
                | MentsuKoutsuKantsu
                | PropAny -- ^ Match anything

-- * Primitives

-- | Value degrades by one if open.
concealedHandDegrade :: Yaku ()
concealedHandDegrade = liftF $ YakuHandConcealedDegrades ()

-- | Must be concealed
concealedHand :: Yaku ()
concealedHand = liftF $ YakuHandConcealed ()

-- | Must be open
openHand :: Yaku ()
openHand = liftF $ YakuHandOpen ()

-- | Yaku that depend on game info. See "YakuInfo".
yakuState :: Yaku YakuInfo
yakuState = liftF (YakuStateful id)

-- | Require any mentsu with a property.
anyKoutsu, anyKantsu, anyShuntsu, anyJantou, anyMentsu, anyKoutsuKantsu, anyMentsuJantou :: MentsuProp -> Yaku ()
anyMentsu        tkind = liftF $ YakuMentsu tkind ()
anyKoutsu        tkind = liftF $ YakuMentsu (MentsuKoutsu       &. tkind) ()
anyShuntsu       tkind = liftF $ YakuMentsu (MentsuShuntsu      &. tkind) ()
anyKantsu        tkind = liftF $ YakuMentsu (MentsuKantsu       &. tkind) ()
anyJantou        tkind = liftF $ YakuMentsu (MentsuJantou       &. tkind) ()
anyKoutsuKantsu  tkind = liftF $ YakuMentsu (MentsuKoutsuKantsu &. tkind) ()
anyMentsuJantou  tkind = liftF $ YakuMentsu (MentsuAnyJantou    &. tkind) ()

-- | Require any mentsu with a property. Rest of the definition may depend
-- on the matched tile.
anyShuntsu', anyKoutsuKantsu', anyMentsu', anyMentsuJantou' :: MentsuProp -> Yaku Tile
anyMentsu'       tkind = liftF $ YakuMentsu' tkind id
anyKoutsuKantsu' tkind = liftF $ YakuMentsu' (MentsuKoutsuKantsu &. tkind) id
anyShuntsu'      tkind = liftF $ YakuMentsu' (MentsuShuntsu      &. tkind) id
anyMentsuJantou' tkind = liftF $ YakuMentsu' (MentsuAnyJantou    &. tkind) id

-- ** Mentsu properties

-- | Binary combinations of mentsu porperties.
(&.), (|.) :: MentsuProp -> MentsuProp -> MentsuProp
(&.) = TileAnd
(|.) = TileOr
infixl 1 &., |.

-- | Tile kinds.
terminal, honor, sangenpai, suited, anyTile, concealed :: MentsuProp
terminal  = TileTerminal
honor     = TileHonor
sangenpai = TileSangenpai
suited    = TileSuited
anyTile   = PropAny
concealed = TileConcealed

sameTile, sameNumber, sameSuit :: Tile -> MentsuProp
sameTile = TileSameAs
sameNumber = TileSameNumber
sameSuit = TileSameSuit

ofNumber :: Number -> MentsuProp
ofNumber = TileNumber

-- | Negation of a MentsuProp.
propNot :: MentsuProp -> MentsuProp
propNot = TileNot

-- * Helpers

-- | Simple yaku helper to require some same property from the four mentsu
-- and any pair.
allMentsuOfKind :: MentsuProp -> Yaku ()
allMentsuOfKind tkind = do
    replicateM_ 4 $ anyMentsu tkind
    anyJantou tkind

