module Handler.Play where

import           Import
-- import qualified GameServer   as GS

import           Data.Time (getCurrentTime)
import qualified Data.Text    as T
import qualified Data.UUID    as UUID
import qualified Data.UUID.V4 as UUID

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
    -- the unique id
    ident  <- T.pack . UUID.toString <$> liftIO UUID.nextRandom

    -- temporary id
    -- tempid <- GS.createGame
    time   <- liftIO getCurrentTime

    let game = Game ident 0 time Nothing
    _ <- runDB $ insert game

    redirect $ GameR ident

getGamesR :: Handler Html
getGamesR = do
    games <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Past Games"
        $(widgetFile "game-history")
