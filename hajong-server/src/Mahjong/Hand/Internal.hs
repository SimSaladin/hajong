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

data HandPublic = HandPublic
    { _handCalled      :: [Mentsu]
    , _handDiscards    :: [Discard]
    , _handRiichi      :: Bool
    , _handDrawWanpai  :: Bool -- ^ Should draw from wanpai
    , _hLastFromWanpai :: Bool
    , _handAgari       :: Maybe Tile
    , _hIppatsu        :: Bool
    , _handAgariCall   :: Maybe Shout
    , _hDoubleRiichi   :: Bool
    , _hIsConcealed    :: Bool -- ^ TODO Updating this
    } deriving (Show, Read, Eq)

data Hand = Hand
    { _handConcealed   :: [Tile]
    , _handPick        :: Maybe Tile
    , _handFuriten     :: Maybe Bool -- ^ Just (temporary?) - TODO updating this field
    , _handPublic      :: HandPublic
    , _hCanTsumo       :: Bool
    } deriving (Show, Read, Eq)

data Discard = Discard
    { _dcTile          :: Tile
    , _dcTo            :: Maybe Kaze
    , _dcRiichi        :: Bool
    } deriving (Show, Read, Eq)

-- * Construct hand

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> Hand
initHand tiles = Hand tiles Nothing Nothing
    (HandPublic [] [] False False False Nothing False Nothing False False)
    False

-- * Lenses

--
makeLenses ''Discard
makeLenses ''HandPublic
makeLenses ''Hand

-- Instances

instance Pretty HandPublic where
    pretty = do
        -- FIXME
        tilenum <- (length . _handCalled) <&> (13 -) . (*3)
        return $ string $ unwords $ replicate tilenum "_"

instance Pretty Hand where
    pretty h =
        prettyList' (_handConcealed h) <+>
        maybe "" (("|-" <+>) . pretty) (_handPick h)
