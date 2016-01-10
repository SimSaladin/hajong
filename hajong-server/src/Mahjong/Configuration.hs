{-# LANGUAGE DeriveGeneric #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Configuration
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Configuration where

------------------------------------------------------------------------------
import           Import
------------------------------------------------------------------------------

data GameSettings = GameSettings { gameTitle :: Text }
                  deriving (Show, Read, Typeable, Generic)

-- | Numerical identifier for players (@[0..3]@). Note that we use 'Kaze'
-- to specifify players in-game (so the logic is simpler), and use `Player'
-- in more general settings (obviously because players change positions
-- between hands).
newtype Player = Player Int deriving (Show, Read, Eq, Ord)

$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''GameSettings)
