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
    ( getGame, getRegisteredUser, getNewAnonUser, G.Game, G.ClientRecord(..)
    , getAcid
    , queryGameServer
    , queryGameServer
    ) where

import Import hiding (update)
import qualified Hajong.Server as G
import qualified Hajong.Database as G
import Data.Acid

getAcid = appGameState <$> getYesod

queryGameServer action = do
    acid <- getAcid
    liftIO $ acid `query` action

updateGameServer action = do
    acid <- getAcid
    liftIO $ acid `update` action


-- | By GameId
getGame :: Int -> Handler (Maybe G.Game)
getGame gid = do
    acid <- getAcid
    liftIO $ acid `query` G.GetGame gid

-- | The game the player is currently up to.
getClientRecord :: Int -> Handler (Maybe G.ClientRecord)
getClientRecord uid = do
    acid <- getAcid
    liftIO $ acid `query` G.GetClientRecord uid

-- | By username, try adding to the server. If full, return Nothing. If
-- success, return (ident, token).
getRegisteredUser, getNewAnonUser :: Text -> Handler (Either Text (Int, G.ClientRecord))
getRegisteredUser user = do
    acid <- getAcid
    token <- liftIO G.randomToken
    liftIO $ acid `update` G.RegisterLoggedInPlayer user token
getNewAnonUser nick = do
    acid <- getAcid
    token <- liftIO G.randomToken
    liftIO $ acid `update` G.RegisterAnonymousPlayer nick token
