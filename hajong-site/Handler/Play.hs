{-# LANGUAGE LambdaCase #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Handler.Play
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Handler.Play where

import           Import
import qualified GameServer   as G

import           Data.Time (getCurrentTime)
import qualified Data.Text    as T
import qualified Data.UUID    as UUID
import qualified Data.UUID.V4 as UUID

-- | Attempt to join the game
getPlayR :: Int -> Handler Html
getPlayR ident = do
    G.getGame ident >>= \case
        Nothing -> notFound
        x -> playLayout x

getLobbyR :: Handler Html
getLobbyR = playLayout Nothing

playLayout :: Maybe G.Game -> Handler Html
playLayout mgid = do
    playerIdent <- maybe Null String <$> lookupSession "ident"
    authToken   <- maybe Null String <$> lookupSession "token"
    defaultLayout $ do
        setTitle "Playing"
        addScriptRemote "http://elm-lang.org/elm-runtime.js"
        addScript $ StaticR js_elm_game_js
        $(widgetFile "play")

getGameR, postGameR :: Int -> Handler Html
getGameR        = postGameR
postGameR ident = do
    let v = undefined -- TODO
    defaultLayout $ do
        setTitle $ toHtml $ "Configure game " <> show ident
        $(widgetFile "game-conf")

postNewGameR :: Handler Html
postNewGameR = do
    -- uuid
    _ident  <- T.pack . UUID.toString <$> liftIO UUID.nextRandom
    {-
    time   <- liftIO getCurrentTime
    let game = Game ident 0 time Nothing
    _ <- runDB $ insert game
    -}

    redirect $ GameR undefined -- TODO

getGamesR :: Handler Html
getGamesR = do
    games <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Past Games"
        $(widgetFile "game-history")
