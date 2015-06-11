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
import qualified Hajong.Connections as G

import           Data.Time (getCurrentTime)
import qualified Data.Text    as T
import qualified Data.UUID    as UUID
import qualified Data.UUID.V4 as UUID

-- | Attempt to join the game
getPlayR :: Int -> Handler Html
getPlayR = playLayout . Just

-- | Current game, or if in none then the public lobby.
getLobbyR :: Handler Html
getLobbyR = playLayout Nothing

-- | Maybe game to join to.
playLayout :: Maybe Int -> Handler Html
playLayout mgid = do
    mauth <- maybeAuthId
    websocketURI <- extraServerWs <$> getExtra
    defaultLayout $ do
        setTitle "Playing"
        addScript $ StaticR js_howler_min_js
        addScript $ StaticR js_elm_js
        $(widgetFile "play")
{-# INLINE playLayout #-}

-- | Creating games
getNewGameR, postNewGameR :: Handler Html
getNewGameR  = do
    -- TODO require not already in a game
    (formWidget, formEnctype) <- generateFormPost newGameForm
    defaultLayout $ do
        setTitle "Create a game"
        $(widgetFile "new-game")

postNewGameR = do
    -- uuid <- T.pack . UUID.toString <$> liftIO UUID.nextRandom
    ((FormSuccess settings,_), _) <- runFormPost newGameForm
    G.InternalGameCreated gid <- goGame $ G.InternalNewGame settings
    redirect $ GameR gid

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
