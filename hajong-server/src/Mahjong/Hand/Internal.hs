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

data RiichiState = NoRiichi | Riichi | DoubleRiichi
                 deriving (Show, Read, Eq)

data DrawState = DrawFromWanpai | DrawFromWall | DrawNone
               deriving (Show, Read, Eq)

data PickedTile m = FromWall (m Tile) | FromWanpai (m Tile) | AgariTsumo Tile | AgariCall Tile Kaze | AgariRinshan Tile Kaze
deriving instance Show (PickedTile Maybe)
deriving instance Read (PickedTile Maybe)
deriving instance Eq   (PickedTile Maybe)
deriving instance Show (PickedTile Identity)
deriving instance Read (PickedTile Identity)
deriving instance Eq   (PickedTile Identity)

pickedTile :: PickedTile Identity -> Tile
pickedTile (FromWall (Identity t))   = t
pickedTile (FromWanpai (Identity t)) = t
pickedTile (AgariTsumo t)            = t
pickedTile (AgariCall t _)           = t
pickedTile (AgariRinshan t _)        = t -- TODO this is probably unnecessary?

data FuritenState = NotFuriten | Furiten | TempFuriten
                  deriving (Show, Read, Eq)

data Discard = Discard
    { _dcTile          :: Tile
    , _dcTo            :: Maybe Kaze
    , _dcRiichi        :: Bool
    } deriving (Show, Read, Eq)

data Hand m = Hand
    { _handCalled      :: [Mentsu]                -- ^ Open mentsu
    , _handDiscards    :: [Discard]               -- ^ Discard pool, annotated with tiles called by others
    , _handRiichi      :: RiichiState             -- ^ Maybe in riichi
    , _handIppatsu     :: Bool                    -- ^ Is this ippatsu round
    , _handState       :: DrawState               -- ^ Maybe should draw
    , _handPicks       :: [PickedTile m]          -- ^ Drawn tiles in draw order. True for wanpai.

    , _handConcealed   :: m [Tile]                -- ^ Concealed tiles
    , _handFuriten     :: m FuritenState          -- ^ TODO updating this field
    , _handCanTsumo    :: m Bool                  -- ^ Has concealed complete hand
    }
deriving instance Show (Hand Maybe)
deriving instance Read (Hand Maybe)
deriving instance Eq   (Hand Maybe)
deriving instance Show (Hand Identity)
deriving instance Read (Hand Identity)
deriving instance Eq   (Hand Identity)

-- | All info
type HandA = Hand Identity

-- | Public info only 
type HandP = Hand Maybe

-- * Construct hand

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> HandA
initHand tiles = Hand [] [] NoRiichi False DrawNone [] (pure tiles) (pure NotFuriten) (pure False)

-- * Lenses

--
makeLenses ''Hand
makeLenses ''Discard

-- Instances

instance Pretty HandP where
    pretty = do
        -- FIXME
        tilenum <- (length . _handCalled) <&> (13 -) . (*3)
        return $ string $ unwords $ replicate tilenum "_"

instance Pretty (PickedTile Identity) where
    pretty = pretty . pickedTile

instance Pretty HandA where
    pretty h = prettyList' (runIdentity $ _handConcealed h) <+> "|-" <+> pretty (_handPicks h)