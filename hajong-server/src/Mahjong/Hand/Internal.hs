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
import           Data.SafeCopy
------------------------------------------------------------------------------

-- XXX: Should be a flag
data RiichiState = NoRiichi | Riichi | DoubleRiichi
                 deriving (Show, Read, Eq)
deriveSafeCopy 0 'base ''RiichiState

-- XXX: Should be flag
data FuritenState = NotFuriten | Furiten | TempFuriten
                  deriving (Show, Read, Eq)
deriveSafeCopy 0 'base ''FuritenState

data DrawState = DrawFromWanpai | DrawFromWall | DrawNone
               deriving (Show, Read, Eq)
deriveSafeCopy 0 'base ''DrawState

data Agari = AgariCall Shout
           | AgariTsumo Tile Bool
           deriving (Show, Read, Eq)
deriveSafeCopy 0 'base ''Agari

agariTile :: Agari -> Tile
agariTile (AgariCall shout) = shoutTile shout
agariTile (AgariTsumo tile _) = tile

-- * PickedTile

-- | Agari tiles do belong to hand; tiles in the shoutTo-field do not.
data PickedTile_0 m = FromWall_0 (m Tile)
                  | FromWanpai_0 (m Tile)
                  | AgariTsumo_0 Tile
                  | AgariTsumoWanpai_0 Tile
                  | AgariCall_0 Shout -- ^ The shout is present in handCalled-field too.

data PickedTile m = PickedTile (m Tile) Bool

pickedTile :: PickedTile Identity -> Tile
pickedTile (PickedTile (Identity t) _) = t

deriving instance Show (PickedTile Maybe)
deriving instance Read (PickedTile Maybe)
deriving instance Show (PickedTile Identity)
deriving instance Read (PickedTile Identity)

instance SafeCopy (m Tile) => SafeCopy (PickedTile_0 m) where
    version = 0
    putCopy (FromWall_0 t)         = contain $ do safePut (0 :: Word8); safePut t
    putCopy (FromWanpai_0 t)       = contain $ do safePut (1 :: Word8); safePut t
    putCopy (AgariTsumo_0 t)       = contain $ do safePut (2 :: Word8); safePut t
    putCopy (AgariCall_0 s)        = contain $ do safePut (3 :: Word8); safePut s
    putCopy (AgariTsumoWanpai_0 t) = contain $ do safePut (4 :: Word8); safePut t
    getCopy = contain $ do tag <- safeGet
                           case tag :: Word8 of
                               0 -> FromWall_0 <$> safeGet
                               1 -> FromWanpai_0 <$> safeGet
                               2 -> AgariTsumo_0 <$> safeGet
                               3 -> AgariCall_0 <$> safeGet
                               4 -> AgariTsumoWanpai_0 <$> safeGet
                               _ -> fail $ "Couldn't identify tag " ++ show tag

instance (Monad m, SafeCopy (m Tile)) => SafeCopy (PickedTile m) where
    version = 1
    kind = extension
    putCopy (PickedTile tile bool) = contain $ do safePut tile; safePut bool
    getCopy = contain $ PickedTile <$> safeGet <*> safeGet

instance (SafeCopy (m Tile), Monad m) => Migrate (PickedTile m) where
    type MigrateFrom (PickedTile m) = PickedTile_0 m
    migrate pick = PickedTile (maybePickedTile pick) False -- XXX: wanpai info discarded

-- migrations only
maybePickedTile :: Monad m => PickedTile_0 m -> m Tile
maybePickedTile (FromWall_0 t)   = t
maybePickedTile (FromWanpai_0 t) = t
maybePickedTile (AgariTsumo_0 t) = return t
maybePickedTile (AgariTsumoWanpai_0 t) = return t
maybePickedTile (AgariCall_0 s) = return $ shoutTile s

-- * Flags

data HandFlag = HandFirsRoundUninterrupted
              deriving (Show, Read, Eq, Ord)
deriveSafeCopy 0 'base ''HandFlag

-- * Discards

data Discard = Discard
    { _dcTile          :: Tile
    , _dcTo            :: Maybe Kaze
    , _dcRiichi        :: Bool
    } deriving (Show, Read)
deriveSafeCopy 0 'base ''Discard

-- * Hand

data Hand_V0 m = Hand_V0
    { _v0_handCalled      :: [Mentsu]         -- ^ Open mentsu
    , _v0_handDiscards    :: [Discard]        -- ^ Discard pool, annotated with tiles called by others
    , _v0_handRiichi      :: RiichiState      -- ^ Maybe in riichi
    , _v0_handIppatsu     :: Bool             -- ^ Is this ippatsu round
    , _v0_handState       :: DrawState        -- ^ Maybe should draw
    , _v0_handPicks       :: [PickedTile m]   -- ^ Drawn tiles in draw order. True for wanpai.
    , _v0_handConcealed   :: m [Tile]         -- ^ Concealed tiles
    , _v0_handFuriten     :: m FuritenState   -- XXX: should be flag
    , _v0_handCanTsumo    :: m Bool           -- XXX: should be flag
    , _v0_handFlags       :: m (Set HandFlag) -- ^ A set of flags active in the hand
    }

data Hand m = Hand
    { _handConcealed   :: m [Tile]         -- ^ Concealed tiles
    , _handCalled      :: [Mentsu]         -- ^ Open mentsu
    , _handPicks       :: [PickedTile m]   -- ^ Drawn tiles in draw order. True for wanpai.
    , _handAgari       :: Maybe Agari
    , _handDiscards    :: [Discard]        -- ^ Discard pool, annotated with tiles called by others
    , _handState       :: DrawState        -- ^ Maybe should draw

    , _handIppatsu     :: Bool             -- ^ Is this ippatsu round
    , _handFuriten     :: m FuritenState   -- XXX: should be flag
    , _handCanTsumo    :: m Bool           -- XXX: should be flag
    , _handRiichi      :: RiichiState      -- ^ Maybe in riichi
    , _handFlags       :: m (Set HandFlag) -- ^ A set of flags active in the hand
    }
deriving instance Show (Hand Maybe)
deriving instance Read (Hand Maybe)
deriving instance Show (Hand Identity)
deriving instance Read (Hand Identity)

instance (SafeCopy (m (Set HandFlag)), SafeCopy (m Bool), SafeCopy (m [Tile]), SafeCopy (m FuritenState), SafeCopy (PickedTile m)) => SafeCopy (Hand_V0 m) where
    putCopy Hand_V0{..} = contain $ do safePut _v0_handCalled; safePut _v0_handDiscards; safePut _v0_handRiichi; safePut _v0_handIppatsu; safePut _v0_handState; safePut _v0_handPicks; safePut _v0_handConcealed; safePut _v0_handFuriten; safePut _v0_handCanTsumo; safePut _v0_handFlags
    getCopy             = contain $ Hand_V0 <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

instance (SafeCopy (m (Set HandFlag)), SafeCopy (m Bool), SafeCopy (m [Tile]), SafeCopy (m FuritenState), SafeCopy (PickedTile m)) => SafeCopy (Hand m) where
    version          = 1
    kind             = extension
    putCopy Hand{..} = contain $ do safePut _handConcealed; safePut _handCalled; safePut _handPicks; safePut _handAgari; safePut _handDiscards; safePut _handState; safePut _handIppatsu; safePut _handFuriten; safePut _handCanTsumo; safePut _handRiichi; safePut _handFlags
    getCopy          = contain $ Hand <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

instance (SafeCopy (m (Set HandFlag)), SafeCopy (m Bool), SafeCopy (m [Tile]), SafeCopy (m FuritenState), SafeCopy (PickedTile m)) => Migrate (Hand m) where
    type MigrateFrom (Hand m) = Hand_V0 m
    migrate Hand_V0{..} = Hand _v0_handConcealed _v0_handCalled _v0_handPicks Nothing _v0_handDiscards _v0_handState _v0_handIppatsu _v0_handFuriten _v0_handCanTsumo _v0_handRiichi _v0_handFlags

-- | All info
type HandA = Hand Identity

-- | Public info only
type HandP = Hand Maybe

-- * Construct hand

-- | A hand that contains provided tiles in starting position
initHand :: [Tile] -> HandA
initHand tiles = Hand (pure tiles) [] [] Nothing [] DrawNone False (pure NotFuriten) (pure False) NoRiichi (pure $ setFromList [HandFirsRoundUninterrupted])

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
