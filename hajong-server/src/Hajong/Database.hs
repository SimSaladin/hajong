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
    module Hajong.Database,

    -- * Queries
    module Hajong.Database.Queries,

    -- * Data types and lenses
    module Hajong.Database.Types
    ) where

import           Import
import           Hajong.Database.Types
import           Hajong.Database.Queries
------------------------------------------------------------------------------
import           Data.ReusableIdentifiers
------------------------------------------------------------------------------

-- | Initialize an empty database. Allows 1024 active clients and 256 running games.
emptyDB :: ServerDB
emptyDB = ServerDB (newRecord 1024) mempty mempty (newRecord 256) mempty mempty
