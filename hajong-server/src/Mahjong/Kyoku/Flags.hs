------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Kyoku.Flags
-- Copyright      : (C) 2015 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2015-12-15T20:47:28+0200
--
-- This module defines common flags carried around in the kyoku and hand
-- states.
------------------------------------------------------------------------------
module Mahjong.Kyoku.Flags where

import           Import
import           Mahjong.Tiles

-- | Extendable data type which defines flags that can be used in the game
-- engine.
data Flag = FirstRoundUninterrupted -- ^ Tenhou, chiihou and renhou are yielded if someone goes out when this flag is in effect.
          | OpenedUraDora [TileEq]
          deriving (Eq, Ord, Read, Show)

$(deriveSafeCopy 0 'base ''Flag)
