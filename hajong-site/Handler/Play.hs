module Handler.Play where

import Import
import Data.Time (getCurrentTime)

getPlayR :: GameIdent -> Handler Html
getPlayR ident = do
    Entity _ v <- runDB $ getBy404 $ UniqueGame ident
    playLayout (Just v)

getLobbyR :: Handler Html
getLobbyR = playLayout Nothing

playLayout :: Maybe Game -> Handler Html
playLayout mg = defaultLayout $ do
    setTitle $ maybe "Lobby" (\g -> toHtml $ "Game " <> gameIdent g) mg
    addScriptRemote "http://elm-lang.org/elm-runtime.js"
    addScript $ StaticR js_elm_game_js
    $(widgetFile "play")

getGameR, postGameR :: GameIdent -> Handler Html
getGameR        = postGameR
postGameR ident = do
    Entity _ v <- runDB $ getBy404 $ UniqueGame ident
    defaultLayout $ do
        setTitle $ toHtml $ "Configure game " <> ident
        $(widgetFile "game-conf")

postNewGameR :: Handler Html
postNewGameR = do
    ident <- _
    time <- liftIO getCurrentTime

    let game = Game ident _ time Nothing
    _ <- runDB $ insert game

    redirect $ GameR ident

getGamesR :: Handler Html
getGamesR = do
    games <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Past Games"
        $(widgetFile "game-history")
