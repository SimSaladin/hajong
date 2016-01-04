------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Database
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Every function here has a corresponding acidic constructor, see below.
------------------------------------------------------------------------------
module Hajong.Database
    ( 
    emptyDB,
    -- * Queries

    -- ** Clients and players
    getClientRecord, connectClient, partClient, registerAnonymousPlayer,
    registerLoggedInPlayer, setPlayerGame,

    -- ** Games
    getGame, getGames, insertGame, destroyGame,

    -- ** Workers
    logWorkerResult, getWorkerResultLog, flushWorkerLog,

    -- ** Other
    dumpDB,

    -- * The ACID constructors
    module Hajong.Database,

    -- * Data types and lenses
    module Hajong.Database.Types

    ) where

import           Hajong.Database.Types
import           Hajong.Database.Queries
------------------------------------------------------------------------------
import           Data.Acid
------------------------------------------------------------------------------

$(makeAcidic ''ServerDB
    [ 'getClientRecord, 'getGame, 'getGames, 'dumpDB, 'connectClient,
    'partClient, 'registerAnonymousPlayer, 'registerLoggedInPlayer,
    'setPlayerGame, 'insertGame, 'setGame, 'destroyGame, 'logWorkerResult,
    'getWorkerResultLog, 'flushWorkerLog ])
