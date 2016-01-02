------------------------------------------------------------------------------
-- | 
-- Module         : Handler.Personal
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2016-01-01T17:40:21+0200
------------------------------------------------------------------------------
module Handler.Personal where

import           Import

-- | User's pages
getPersonalR :: Handler Html
getPersonalR = do
    (_uid, Entity userId User{..}) <- requireAuthPair
    res <- runDB $ selectList [SavedRoundStateUser ==. userId] [Desc SavedRoundStateCreated]
    defaultLayout $ do
        setTitle "Me"
        addScript $ StaticR js_elm_js
        $(widgetFile "personal")
