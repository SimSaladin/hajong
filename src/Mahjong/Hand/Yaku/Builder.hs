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
module Mahjong.Hand.Yaku.Builder where

import           Control.Monad.Free
import           Control.Monad.State

import           Mahjong.Hand.Mentsu
import           Mahjong.Tiles (Tile(..), Number(..))
import qualified Mahjong.Tiles as T

data Yaku = Yaku
          { yakuHan :: Int
          , yakuName :: Text
          } deriving (Show, Read)

-- | Required info to calculate the value from a hand.
data ValueInfo = ValueInfo
              { vRound :: T.Kaze
              , vPlayer :: T.Kaze
              , vRiichi :: Bool
              , vConcealed :: Bool
              , vDiscarded :: [Tile] -- ^ To check furiten
              , vMentsu :: [Mentsu]
              , vWinWith :: Tile
              } deriving (Show, Read)

-- * YakuCheck

type YakuCheck = Free Check

data Check next = YakuMentsu MentsuProp next
                -- ^ Require a simple mentsu property. Requiring this
                -- first could allow for simple optimization by
                -- removing some repeated checking.

                | YakuMentsu' MentsuProp (Tile -> next)
                -- ^ Require a mentsu property, but allow upcoming
                -- properties depend on the matched tile.

                | YakuStateful (ValueInfo -> next)
                -- ^ Depend on game state.

                | YakuHandConcealedDegrades next
                | YakuHandConcealed next
                | YakuHandOpen next
                deriving (Functor)

runYakuCheck :: ValueInfo -> YakuCheck Yaku -> Maybe Yaku
runYakuCheck info = fmap fst . (`runStateT` vMentsu info) . iterM f
    where
        f :: Check (StateT [Mentsu] Maybe Yaku) -> StateT [Mentsu] Maybe Yaku
        f (YakuMentsu  mp s)            = get >>= lift . findMatch mp >>= putRes >>  s
        f (YakuMentsu' mp s)            = get >>= lift . findMatch mp >>= putRes >>= s
        f (YakuStateful s)              = s info
        f (YakuHandConcealedDegrades s) = if vConcealed info then s else (\x -> x { yakuHan = yakuHan x - 1 }) <$> s
        f (YakuHandConcealed s)         = if vConcealed info then s else lift Nothing
        f (YakuHandOpen s)              = if vConcealed info then lift Nothing else s

        putRes (xs, t) = put xs >> return t

-- ** @MentsuProp@s

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

-- | Binary combinations of mentsu porperties.
(&.), (|.) :: MentsuProp -> MentsuProp -> MentsuProp
(&.) = TileAnd
(|.) = TileOr
infixl 1 &., |.

-- ** @Check@ primitives

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
-- and any pair.
allMentsuOfKind :: MentsuProp -> YakuCheck ()
allMentsuOfKind tkind = do
    replicateM_ 4 $ anyMentsu tkind
    anyJantou tkind

-- | Require any mentsu with a property.
anyKoutsu, anyKantsu, anyShuntsu, anyJantou, anyMentsu, anyKoutsuKantsu, anyMentsuJantou :: MentsuProp -> YakuCheck ()
anyMentsu        tkind = liftF $ YakuMentsu tkind ()
anyKoutsu        tkind = liftF $ YakuMentsu (MentsuKoutsu       &. tkind) ()
anyShuntsu       tkind = liftF $ YakuMentsu (MentsuShuntsu      &. tkind) ()
anyKantsu        tkind = liftF $ YakuMentsu (MentsuKantsu       &. tkind) ()
anyJantou        tkind = liftF $ YakuMentsu (MentsuJantou       &. tkind) ()
anyKoutsuKantsu  tkind = liftF $ YakuMentsu (MentsuKoutsuKantsu &. tkind) ()
anyMentsuJantou  tkind = liftF $ YakuMentsu (MentsuAnyJantou    &. tkind) ()

-- | Require any mentsu with a property. Rest of the definition may depend
-- on the matched tile.
anyShuntsu', anyKoutsuKantsu', anyMentsu', anyMentsuJantou' :: MentsuProp -> YakuCheck Tile
anyMentsu'       tkind = liftF $ YakuMentsu' tkind id
anyKoutsuKantsu' tkind = liftF $ YakuMentsu' (MentsuKoutsuKantsu &. tkind) id
anyShuntsu'      tkind = liftF $ YakuMentsu' (MentsuShuntsu      &. tkind) id
anyMentsuJantou' tkind = liftF $ YakuMentsu' (MentsuAnyJantou    &. tkind) id

-- *** Mentsu properties

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

-- ** Check MentsuProps directly.

-- | Find a match in a list of mentsu. Returns the matches identifier tile and leftovers.
findMatch :: MentsuProp -> [Mentsu] -> Maybe ([Mentsu], Tile)
findMatch _  []   = Nothing
findMatch mp (x:xs)
    | matchProp mp x = Just (xs, unsafeHead $ mentsuTiles x)
    | otherwise      = (_1 %~ (x:)) <$> findMatch mp xs

-- | Match a property on a mentsu.
matchProp :: MentsuProp -> Mentsu -> Bool
matchProp tt mentsu
    | (firstTile:_) <- mentsuTiles mentsu = case tt of
        MentsuJantou       | isJantou mentsu       -> True
        MentsuAnyJantou    | not $ isJantou mentsu -> True
        MentsuShuntsu      | isShuntsu mentsu      -> True
        MentsuKoutsu       | isKoutsu mentsu       -> True
        MentsuKantsu       | isKantsu mentsu       -> True
        MentsuKoutsuKantsu | isKantsu mentsu || isKoutsu mentsu -> True
        -- XXX: this is incomplete (shuntsu + terminals etc.)
        TileTerminal        -> T.terminal firstTile
        TileSameAs tile     -> firstTile == tile
        TileSuited          -> T.isSuited firstTile
        TileSameSuit tile   -> T.suitedSame tile firstTile
        TileSameNumber tile -> T.tileNumber tile      == T.tileNumber firstTile
        TileNumber n        -> T.tileNumber firstTile == Just n
        TileHonor           -> not $ T.isSuited firstTile
        TileSangenpai       -> T.sangenpai firstTile
        TileAnd x y         -> matchProp x mentsu && matchProp y mentsu
        TileOr x y          -> matchProp x mentsu || matchProp y mentsu
        TileNot x           -> not $ matchProp x mentsu
        TileConcealed       -> isNothing $ mentsuShout mentsu
        PropAny             -> True
        _ -> True
    | otherwise = error "ofTileType: empty mentsu"
