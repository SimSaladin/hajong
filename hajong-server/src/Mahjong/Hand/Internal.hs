{-# LANGUAGE UndecidableInstances #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Hand.Internal
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Hand.Internal where

------------------------------------------------------------------------------
import           Import
import           Mahjong.Tiles
import           Mahjong.Hand.Mentsu
------------------------------------------------------------------------------

-- * Types

-- XXX: Should be a flag
data RiichiState = NoRiichi | Riichi | DoubleRiichi
                 deriving (Show, Read, Eq)

-- XXX: Should be flag
data FuritenState = NotFuriten | Furiten | TempFuriten
                  deriving (Show, Read, Eq)

data DrawState = DrawFromWanpai | DrawFromWall | DrawNone
               deriving (Show, Read, Eq)

data Agari = AgariCall Shout
           | AgariTsumo Tile Bool
           deriving (Show, Read, Eq)

data PickedTile = PickedTile { pickedTile :: Tile, pickedWanpai :: Bool }
                deriving (Show, Read)

data HandFlag = HandFirsRoundUninterrupted
              | HandOpen -- ^ Whether the hand has been opened with a non-ron shout
              deriving (Show, Read, Eq, Ord)

data Discard = Discard
    { _dcTile          :: Tile
    , _dcTo            :: Maybe Kaze
    , _dcRiichi        :: Bool
    } deriving (Show, Read)

makeLenses ''Discard

data Hand = Hand
    { _handConcealed   :: [Tile]           -- ^ Concealed tiles
    , _handCalled      :: [Mentsu]         -- ^ Open mentsu
    , _handPicks       :: [PickedTile]   -- ^ Drawn tiles in draw order. True for wanpai.
    , _handAgari       :: Maybe Agari
    , _handDiscards    :: [Discard]        -- ^ Discard pool, annotated with tiles called by others
    , _handState       :: DrawState        -- ^ Maybe should draw
    , _handIppatsu     :: Bool             -- ^ Is this ippatsu round
    , _handFuriten     :: FuritenState   -- XXX: should be flag
    , _handCanTsumo    :: Bool           -- XXX: should be flag
    , _handRiichi      :: RiichiState      -- ^ Maybe in riichi
    , _handFlags       :: Set HandFlag -- ^ A set of flags active in the hand
    } deriving (Show, Read)

makeLenses ''Hand

-- | Public info only
newtype PlayerHand = PlayerHand { fromPlayerHand :: Hand }
                     deriving (Show, Read)

-- * Construct hand

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> Hand
initHand tiles = Hand tiles [] [] Nothing [] DrawNone False NotFuriten False NoRiichi (setFromList [HandFirsRoundUninterrupted])

-- * Functions

isOpen, isConcealed :: Hand -> Bool
isOpen = isHandFlagSet HandOpen
isConcealed = not . isOpen

isHandFlagSet :: HandFlag -> Hand -> Bool
isHandFlagSet flag hand = hand^.handFlags.to (elem flag)

setHandFlag, unsetHandFlag :: HandFlag -> Hand -> Hand
setHandFlag   flag = handFlags%~insertSet flag
unsetHandFlag flag = handFlags%~deleteSet flag

agariTile :: Agari -> Tile
agariTile (AgariCall shout) = shoutTile shout
agariTile (AgariTsumo tile _) = tile

-- Instances

instance Pretty PickedTile where
    pretty = pretty . pickedTile

instance Pretty Hand where
    pretty h = prettyList' (_handConcealed h) <+> "|-" <+> pretty (_handPicks h)

deriveSafeCopy 0 'base ''RiichiState
deriveSafeCopy 0 'base ''FuritenState
deriveSafeCopy 0 'base ''DrawState
deriveSafeCopy 0 'base ''Agari
deriveSafeCopy 0 'base ''PickedTile
deriveSafeCopy 0 'base ''HandFlag
deriveSafeCopy 0 'base ''Discard
deriveSafeCopy 0 'base ''Hand
deriveSafeCopy 0 'base ''PlayerHand
