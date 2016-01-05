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
import Text.Hamlet          (hamletFile)
import qualified GameServer   as G
import qualified Hajong.Connections as G
import qualified Mahjong      as G

import qualified Data.Text    as T
import qualified Data.UUID    as UUID

-- | Attempt to join the game
getPlayR :: Int -> Handler Html
getPlayR = playLayout . Just

-- | Current game, or if in none then the public lobby.
getLobbyR :: Handler Html
getLobbyR = playLayout Nothing

-- | Maybe game to join to.
playLayout :: Maybe Int -> Handler Html
playLayout mgid = do
    token <- maybe "" id . reqToken <$> getRequest
    muserName <- maybeAuthId
    fullscreen <- isJust <$> lookupGetParam "fullscreen"
    websocketURI <- hajongWs . appHajong . appSettings <$> getYesod
    (if fullscreen then fullscreenLayout else defaultLayout) $ do
        setTitle "Playing"
        addScript $ StaticR js_howler_min_js
        addScript $ StaticR js_elm_js
        $(widgetFile "play")
{-# INLINE playLayout #-}

fullscreenLayout :: Widget -> Handler Html
fullscreenLayout widget = do
    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_normalize_css
        addStylesheet $ StaticR css_main_css
        addScript $ StaticR js_jquery_1_11_1_min_js
        addScript $ StaticR js_modernizr_2_6_2_respond_1_1_0_min_js
        widget
    withUrlRenderer $(hamletFile "templates/fullscreen-layout.hamlet")

-- * Create games

-- | Creating games
getNewGameR, postNewGameR :: Handler Html
getNewGameR  = do
    -- TODO require not already in a game
    (formWidget, formEnctype) <- generateFormPost newGameForm
    defaultLayout $ do
        setTitle "Create a game"
        $(widgetFile "new-game")

postNewGameR = do
    ((FormSuccess settings,_), _) <- runFormPost newGameForm

    print settings
    createTime <- liftIO getCurrentTime
    G.InternalGameCreated gid <- goGame $ G.InternalNewGame settings
    Just gs <- G.getGame gid

    runDB $ insert $ Game (UUID.toText $ G._gameUUID gs) createTime Nothing Nothing []
    redirect $ PlayR gid

newGameForm :: Form G.GameSettings
newGameForm = renderDivs $ G.GameSettings
    <$> areq textField "Game title" Nothing

-- | Modify game settings
getGameR, postGameR :: Int -> Handler Html
getGameR        = postGameR
postGameR ident = do
    game <- G.getGame ident >>= maybe notFound return
    defaultLayout $ do
        setTitle $ toHtml $ "Configure game " <> show ident
        $(widgetFile "game-conf")

-- | List all (ended) games
getGamesR :: Handler Html
getGamesR = do
    games <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Past Games"
        $(widgetFile "game-history")

-- | Authenticate for games
getAuthTokenR :: Handler Html
getAuthTokenR = do
    user <- maybeAuthId
    anon <- lookupGetParam "anon"
    case (user, anon) of
        (Just name, _)       -> G.getRegisteredUser name >>= processAuthRes
        (Nothing, Just nick) -> G.getNewAnonUser    nick >>= processAuthRes
        _                    -> do setMessage "That requires login"; redirect $ AuthR LoginR

-- | This set localStorage and redirects.
processAuthRes :: Either Text (Int, G.ClientRecord) -> Handler Html
processAuthRes res = case res of 
    Left err -> do setMessage $ toHtml err; redirect HomeR
    Right (ident, cr) -> defaultLayout $ do
        setTitle "Game authentication"
        $(widgetFile "push-auth-token")
