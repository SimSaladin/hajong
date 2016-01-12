{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- XXX: get rid of this

------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Database.Types
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2016-01-03T20:21:10+0200
--
-- The root type, @ServerDB@ is defined at "Hajong.Database".
------------------------------------------------------------------------------
module Hajong.Database.Types where

------------------------------------------------------------------------------
import           Mahjong
------------------------------------------------------------------------------
import           Data.SafeCopy
import qualified Data.UUID as UUID
------------------------------------------------------------------------------

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
    } deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''ClientRecord)
$(deriveSafeCopy 0 'base ''PastGame)

-- Not our data types, hope they don't change internal rep.
$(deriveSafeCopy 0 'base ''UUID.UUID)

-- | TODO: Move these instances and data types to corresponding .Types modules
$(deriveSafeCopy 0 'base ''Machine)
$(deriveSafeCopy 0 'base ''GameState)

-- * Lenses

--
makeLenses ''ClientRecord
