{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Database.Types
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2016-01-03T20:21:10+0200
------------------------------------------------------------------------------
module Hajong.Database.Types where

------------------------------------------------------------------------------
import           Hajong.Connections
import           Mahjong
------------------------------------------------------------------------------
import           Data.SafeCopy
import           Data.ReusableIdentifiers
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
------------------------------------------------------------------------------

-- | This is the root ACID type.
--
-- Players are identified with unique Ints. When joining the server, they
-- may opt for a new *anonymous* identifier or provide their previous
-- identifier and a passphrase.
data ServerDB = ServerDB
    { _sePlayerRecord  :: Record
    -- ^ Record of used and free integral player (client) identifiers. The
    -- pool is bounded in size, see @Record@.
    , _seReserved      :: IntMap ClientRecord
    -- ^ Player identifiers mapped to @ClientRecord@s. If an identifier is
    -- reserved in _sePlayerRecord, then it must also have a record in this
    -- map.
    , _seNicks         :: Map Nick Int
    -- ^ This is related to anonymous clients which are not working at the
    -- moment. Usernames and visible names are managed on the webapp side.
    , _seGameRecord    :: Record
    -- ^ Integral game identifiers.
    , _seGames         :: IntMap Game
    -- ^ Games that are running at the moment, analogous to @_seReserved@. 
    , _seHistory       :: Map UUID PastGame
    -- ^ History of games that have finished.
    } deriving (Show, Typeable)

-- | Initialize an empty database. Allows 1024 active clients and 256 running games.
emptyDB :: ServerDB
emptyDB = ServerDB (newRecord 1024) mempty mempty (newRecord 256) mempty mempty

-- | A record of a client connected or previously connected.
data ClientRecord = ClientRecord
    { _cNick           :: Text
    -- ^ Better named username; maps to them.
    , _cToken          :: Text 
    -- ^ When a client has disconnected, to reconnect it must know this
    -- token. If the client loses the token, it must retrieve it by
    -- authenticating to the site which gets and sends the value to the
    -- client.
    , _cRegistered     :: Bool
    -- ^ We technically support anonymous players on the game server level,
    -- but atm this isn't used. (anonymous authentication was actually
    -- implemented in an earlier version).
    , _cStatus         :: Either UTCTime UTCTime
    -- ^ Left (disconnected at) or Right (connected at)
    , _cInGame         :: Maybe Int
    -- ^ The game the client has joined to
    } deriving (Show, Typeable)

-- | Serialization of an on-going game.
type Game = GameState Int

-- | A record of a past game.
data PastGame = PastGame
    { _pgResults       :: Either Text FinalPoints
    , _pgGameState     :: Game
    , _pgLastMachine   :: Machine
    } deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''ServerDB)
$(deriveSafeCopy 0 'base ''ClientRecord)
$(deriveSafeCopy 0 'base ''FinalPoints)
$(deriveSafeCopy 0 'base ''PastGame)

-- Not our data types, hope they don't change internal rep.
$(deriveSafeCopy 0 'base ''Identity)
$(deriveSafeCopy 0 'base ''UUID.UUID)

-- | TODO: Move these instances and data types to corresponding .Types modules
$(deriveSafeCopy 0 'base ''Machine)
$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''Wanpai)
$(deriveSafeCopy 0 'base ''GameSettings)
$(deriveSafeCopy 0 'base ''Tile)
$(deriveSafeCopy 0 'base ''TileEq)
$(deriveSafeCopy 0 'base ''MentsuKind)
$(deriveSafeCopy 0 'base ''Honor)
$(deriveSafeCopy 0 'base ''Number)
$(deriveSafeCopy 0 'base ''Sangen)
$(deriveSafeCopy 0 'base ''TileKind)
$(deriveSafeCopy 0 'base ''KyokuResults)
$(deriveSafeCopy 0 'base ''AbortiveDraw)
$(deriveSafeCopy 0 'base ''Flag)
$(deriveSafeCopy 0 'base ''Value)
$(deriveSafeCopy 0 'base ''Discard)
$(deriveSafeCopy 0 'base ''TurnAction)
$(deriveSafeCopy 0 'base ''Yaku)
$(deriveSafeCopy 0 'base ''Mentsu)
$(deriveSafeCopy 0 'base ''ShoutKind)
$(deriveSafeCopy 0 'base ''Kaze)
$(deriveSafeCopy 0 'base ''ValuedHand)
$(deriveSafeCopy 0 'base ''Shout)
$(deriveSafeCopy 0 'base ''GameEvent)
$(deriveSafeCopy 0 'base ''RiichiState)
$(deriveSafeCopy 0 'base ''DrawState)
$(deriveSafeCopy 0 'base ''FuritenState)
$(deriveSafeCopy 0 'base ''HandFlag)
$(deriveSafeCopy 0 'base ''GameState)

-- SafeCopy instances for indexed types

instance (SafeCopy (m (Set HandFlag)), SafeCopy (m Bool), SafeCopy (m [Tile]), SafeCopy (m FuritenState), SafeCopy (PickedTile m)) => SafeCopy (Hand m) where
    version          = 0
    putCopy Hand{..} = contain $ do safePut _handCalled; safePut _handDiscards; safePut _handRiichi; safePut _handIppatsu; safePut _handState; safePut _handPicks; safePut _handConcealed; safePut _handFuriten; safePut _handCanTsumo; safePut _handFlags
    getCopy          = contain $ Hand <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy (m Tile) => SafeCopy (PickedTile m) where
    version = 0
    putCopy (FromWall t)         = contain $ do safePut (0 :: Word8); safePut t
    putCopy (FromWanpai t)       = contain $ do safePut (1 :: Word8); safePut t
    putCopy (AgariTsumo t)       = contain $ do safePut (2 :: Word8); safePut t
    putCopy (AgariCall s)        = contain $ do safePut (3 :: Word8); safePut s
    putCopy (AgariTsumoWanpai t) = contain $ do safePut (4 :: Word8); safePut t
    getCopy = contain $ do tag <- safeGet
                           case tag :: Word8 of
                               0 -> FromWall <$> safeGet
                               1 -> FromWanpai <$> safeGet
                               2 -> AgariTsumo <$> safeGet
                               3 -> AgariCall <$> safeGet
                               4 -> AgariTsumoWanpai <$> safeGet
                               _ -> fail $ "Couldn't identify tag " ++ show tag

instance SafeCopy (Hand m) => SafeCopy (Kyoku' m) where
    version = 0
    putCopy Kyoku{..} = contain $ do safePut _pRound; safePut _pTurn; safePut _pFlags; safePut _pOja; safePut _pFirstOja; safePut _pWallTilesLeft; safePut _pDora; safePut _pPlayers; safePut _pHonba; safePut _pRiichi; safePut _pResults; safePut _sEventHistory; safePut _sHands; safePut _sWall; safePut _sWanpai; safePut _sWaiting;
    getCopy = contain $ do Kyoku <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

-- * Lenses

--
makeLenses ''ServerDB
makeLenses ''ClientRecord
