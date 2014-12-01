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
getPlayR ident = G.getGame ident >>= maybe notFound (playLayout . Just)

getLobbyR :: Handler Html
getLobbyR = playLayout Nothing

playLayout :: Maybe G.Game -> Handler Html
playLayout mgid = do
    mauth <- maybeAuthId
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
    -- new uuid
    _ident  <- T.pack . UUID.toString <$> liftIO UUID.nextRandom
    redirect $ GameR undefined -- TODO

getGamesR :: Handler Html
getGamesR = do
    games <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Past Games"
        $(widgetFile "game-history")

getAuthTokenR :: Handler Html
getAuthTokenR = do
    user <- maybeAuthId
    anon <- lookupGetParam "anon"
    case (user, anon) of
        (Just name, _)       -> G.getRegisteredUser name >>= processAuthRes
        (Nothing, Just nick) -> G.getNewAnonUser    nick >>= processAuthRes
        _                    -> do setMessage "That requires login"; redirect $ AuthR LoginR

processAuthRes :: Either Text (Int, G.ClientRecord) -> Handler Html
processAuthRes res = case res of 
    Left err -> do setMessage $ toHtml err; redirect HomeR
    Right (ident, cr) -> defaultLayout $ do
        setTitle "Game authentication"
        $(widgetFile "push-auth-token")
