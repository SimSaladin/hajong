{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import qualified Yesod.Auth.Account as Acc
import Yesod.Auth.Facebook.ServerSide
import qualified Facebook as FB
import qualified Yesod.Facebook as YF
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)
import Data.Acid
import qualified Hajong.Server as G
import qualified Hajong.Connections as G
import Control.Concurrent.Lock
import Control.Concurrent.MVar
import qualified Network.WebSockets as WS

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    , appGameState :: AcidState G.ServerDB
    , appGameLock :: Lock
    , gameIn :: MVar G.InternalEvent
    , gameOut :: MVar G.InternalResult
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        480    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        route <- getCurrentRoute
        muser <- maybeAuthId

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_normalize_css
            addStylesheet $ StaticR css_main_css
            addScript $ StaticR js_jquery_1_11_1_min_js
            addScript $ StaticR js_modernizr_2_6_2_respond_1_1_0_min_js
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized _ _ = requireAuthId >> return Authorized

    addStaticContent =
        addStaticContentExternal (if development then Right else minifym) genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    jsLoader _ = BottomOfBody

    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = Acc.Username -- :: Text
    getAuthId (Creds "account" username _) = return (Just username)
    getAuthId (Creds "fb" _ _) = lookupSession "username"
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [authFacebook ["email"], Acc.accountPlugin]
    authHttpManager _ = error "No manager needed"
    onLogin = do
        -- setMessageI NowLoggedIn

        -- If via fb: create the User persist entry if it doesn't exist.
        -- If it exists, update fbUserId <-- TODO
        -- Set session key "username"
        token <- getUserAccessToken
        case token of
            Nothing -> return ()
            Just token -> do
                fbUser <- YF.runYesodFbT $ FB.getUser "me" [] (Just token)
                let Just userEmail = FB.userEmail fbUser
                    uid = FB.idCode $ FB.userId fbUser

                mUserInDB <- runDB $ getBy $ UniqueUserEmail userEmail
                userInDB <- case mUserInDB of
                    Nothing -> let newUser = User uid "" userEmail (Just uid) True "" ""
                                   in runDB (insert newUser) >> return newUser
                    Just (Entity _ v) -> return v
                setSession "username" (userUsername userInDB)

        return ()
    maybeAuthId = lookupSession credsKey

instance Acc.YesodAuthAccount (Acc.AccountPersistDB App User) App where
    runAccountDB = Acc.runAccountPersistDB

instance Acc.AccountSendEmail App

instance YF.YesodFacebook App where
    fbCredentials = extraFbCredentials . appExtra . settings
    fbHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

goGame :: G.InternalEvent -> Handler G.InternalResult
goGame ev = do
    App{..} <- getYesod
    liftIO $ with appGameLock $ do
        putMVar gameIn ev
        takeMVar gameOut
