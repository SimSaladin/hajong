------------------------------------------------------------------------------
-- | 
-- Module         : Handler.View
-- Copyright      : (C) 2015 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2015-12-30T18:29:13+0200
------------------------------------------------------------------------------
module Handler.View where

import           Import
import qualified Mahjong as G

import qualified Data.Aeson as A
import           Text.Julius (RawJS(..))

-- | View with the given UUID/kyoku
getViewR :: Text -> Text -> Int -> Int -> Handler Html
getViewR uuid k_in n h = do
    k    <- maybe notFound return (readMay k_in) :: Handler G.Kaze
    game <- runDB $ getBy404 $ UniquePastGame uuid

    defaultLayout $ do
        setTitle "View"
        addScript $ StaticR js_elm_js
        $(widgetFile "view-game")

getListViewsR :: Handler Value
getListViewsR = do
    uid <- requireUserId
    res <- runDB $ selectList [SavedRoundStateUser ==. uid] [Desc SavedRoundStateCreated]
    returnJson res

-- * API

postSaveViewR :: Handler Value
postSaveViewR = do
    st <- SavedRoundState
            <$> liftIO getCurrentTime
            <*> requireUserId
            <*> fmap (toStrict . decodeUtf8 . A.encode :: A.Value -> Text) requireJsonBody
    key <- runDB $ insert st
    returnJson key
