{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Handler.Status
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Handler.Status where

import Import
import GameServer as G
import Hajong.Server as G
import Data.Acid
import qualified Data.Map as M
import qualified Data.IntMap as IM

getStatusR :: Handler Html
getStatusR = do
    ServerDB{..} <- G.getAcid >>= liftIO . (`query` G.DumpDB)
    defaultLayout $ do
        setTitle "System status"
        $(widgetFile "status")
