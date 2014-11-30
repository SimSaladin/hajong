------------------------------------------------------------------------------
-- | 
-- Module         : GameServer
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GameServer
    ( getGame, getToken
    , G.Game ) where

import Import
import qualified Hajong.Server as G
import Data.Acid

getGame :: Int -> Handler (Maybe G.Game)
getGame gid = do
    app <- getYesod
    let acid = appGameState app
    liftIO $ acid `query` G.GetGame gid

getToken :: Handler (Maybe Text)
getToken = do
    iam <- lookupSession "ident"
    undefined
