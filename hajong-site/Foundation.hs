{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import Yesod.Auth.Message as Msg
import qualified Yesod.Auth.Account as Acc
import qualified Yesod.Auth.Account.Message as AccMsg
import Yesod.Auth.Facebook.ServerSide
import qualified Facebook as FB
import qualified Yesod.Facebook as YF

import Language.Haskell.TH (runIO, litE, stringL)
import Data.Acid
import qualified Hajong.Database as G
import qualified Hajong.Connections as G
import Control.Concurrent.Lock
import qualified Network.WebSockets as WS

import Handler.SendMail
import qualified Network.Mail.Mime as Mime

import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text (strip)
import Data.Digest.Pure.MD5 (md5)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appGameState   :: AcidState G.ServerDB
    , appGameLock    :: Lock
    , appGameIn      :: MVar G.InternalEvent
    , appGameOut     :: MVar G.InternalResult
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        (14 * 24 * 60)    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- The defaultCsrfMiddleware:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        route <- getCurrentRoute
        mapair <- maybeAuthPair

        let development =
#if DEVELOPMENT
                True
#else
                False
#endif

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_normalize_css
            addStylesheet $ StaticR css_main_css
            addScript $ StaticR js_jquery_1_11_1_min_js
            addScript $ StaticR js_modernizr_2_6_2_respond_1_1_0_min_js
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized SupportR _ = return Authorized
    isAuthorized SupportThankYouR _ = return Authorized
    isAuthorized _ _ = requireAuthId >> return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger
    jsLoader _ = BottomOfBody

	--  TODO
    -- urlRenderOverride y (StaticR s) =
    --     Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    -- urlRenderOverride _ _ = Nothing

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB (appDatabaseConf . appSettings) appConnPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

-- * Email

instance YesodSES App where
    getSES = getsYesod $ appSES . appSettings

appMailFooter :: Text
appMailFooter = unlines
    [ "\n\n-------------------------------"
    , "This is an automatic email from funjong.org. You can reply to this"
    , "address."
    , ""
    , "    http://funjong.org" ]

instance Acc.AccountSendEmail App where
    sendVerifyEmail username addr url = do
        Entity _ User{..} <- runDB $ getBy404 $ UniqueUsername username

        let subject = "Verify your email address"
            body = unlines
                [ "Welcome, " ++ userDisplayName ++ "!"
                , ""
                , "You have created an account at funjong.org. Your username is: " ++ userUsername
                , "Follow the link below to verify your email address"
                , ""
                , "    " ++ url
                , ""
                , "If you have any questions, please contact 'info at funjong dot org' or"
                , "file a support request at http://funjong.org/support."
                , appMailFooter ]

        let mail = Mime.simpleMail' (Address (Just userDisplayName) addr)
                                  (error "supplied elsewhere") subject (fromStrict body)
        renderSendMail [addr] mail

    sendNewPasswordEmail username addr url = do
        Entity _ User{..} <- runDB $ getBy404 $ UniqueUsername username

        let subject = "Password reset request"
            body = unlines
                [ "Someone requested a password request for this address on funjong.org."
                , ""
                , "Click the link below to reset your password:"
                , "\n    " ++ url ++ "\n"
                , "If you did not request a password reset, you may safely ignore this email."
                , appMailFooter ]

        let mail = Mime.simpleMail' (Address (Just userDisplayName) addr)
                                    (error "npplied elsewhere") subject (fromStrict body)
        renderSendMail [addr] mail

-- * Auth

loggedIn :: Handler Bool
loggedIn = isJust <$> maybeAuthId

myLoginWidget :: Widget
myLoginWidget = do
        setTitleI Msg.LoginTitle
        master <- getYesod
        let [fb, acc] = map (flip apLogin AuthR) $ authPlugins master
        [whamlet|
<div.auth-wrapper>
    ^{fb}
    <hr>
    ^{acc}
|]

requireUserId :: Handler UserId
requireUserId = fmap (entityKey . snd) requireAuthPair

instance YesodAuth App where
    type AuthId App = Acc.Username -- :: Text
    getAuthId (Creds "account" username _) = return (Just username)
    getAuthId c@(Creds "fb" cId cExtra) = do

        -- create the User persist entry if it doesn't exist.
        -- If it exists, update fbUserId <-- TODO
        token <- getUserAccessToken
        case token of
            Nothing    -> return Nothing
            Just token -> do

                fbUser <- YF.runYesodFbT $ FB.getUser "me" [("fields", "name,email")] (Just token)

                let fbUserId    = FB.idCode $ FB.userId fbUser
                    fbUserEmail = maybe (fbUserId `mappend` "@facebook.com") id $ FB.userEmail fbUser
                    displayName = maybe fbUserId id $ FB.userName fbUser

                mUserInDB <- runDB $ selectFirst [UserFbUserId ==. Just fbUserId] []

                userInDB <- case mUserInDB of
                    Nothing -> let newUser = (Acc.userCreate displayName fbUserEmail "" "")
                                        { userUsername = fbUserId
                                        , userVerified = True
                                        , userFbUserId = Just fbUserId }
                                   in runDB (insert newUser) >> return newUser
                    Just (Entity _ v) -> return v
                return $ Just (userUsername userInDB)

    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [ myFacebookPlugin, myAccountPlugin ]
    loginHandler = lift . authLayout $ myLoginWidget
    authHttpManager _ = error "No manager needed"

    -- onLogin = setMessageI NowLoggedIn
    onLogout = setMessage "You have logged out"
    maybeAuthId = lookupSession credsKey

instance YesodAuthPersist App where
    type AuthEntity App = Entity User
    getAuthEntity = runDB . getBy . UniqueUsername

-- ** Account

myAccountPlugin :: AuthPlugin App
myAccountPlugin = Acc.accountPlugin { apLogin = myAccountLoginWidget }
    where
        myAccountLoginWidget tm = do
            ((_,widget), enctype) <- liftHandlerT $ runFormPost $ renderDivs Acc.loginForm
            [whamlet|
<div .login-account>
  <form method=post enctype=#{enctype} action=@{tm Acc.loginFormPostTargetR}>
    ^{widget}
    <input.btn-full type=submit value=_{LoginTitle}>
    <p>
        <a.btn.btn-alt.btn-half href="@{tm Acc.newAccountR}">_{Msg.RegisterLong}
        <a.btn.btn-alt.btn-half href="@{tm Acc.resetPasswordR}">_{AccMsg.MsgForgotPassword}
|]

myResetPasswordWidget :: Widget
myResetPasswordWidget = do
    muname <- liftHandlerT maybeAuthId

    let myResetPasswordForm = areq textField userSettings muname
        userSettings        = FieldSettings (SomeMessage AccMsg.MsgUsername) Nothing (Just "username") Nothing []

    ((_,widget), enctype) <- liftHandlerT $ runFormPost $ renderDivs myResetPasswordForm
    [whamlet|
<div .auth-wrapper>
  <h1>Reset password
  <div .resetPasswordDiv>
    <form method=post enctype=#{enctype} action=@{AuthR Acc.resetPasswordR}>
        ^{widget}
        <input.btn.btn-full type=submit value=_{Msg.SendPasswordResetEmail}>
|]

instance Acc.YesodAuthAccount (Acc.AccountPersistDB App User) App where
    runAccountDB = Acc.runAccountPersistDB
    getNewAccountR = lift $ do
        whenM loggedIn $ do setMessage "You are already logged in" >> redirect HomeR
        ((_,widget), enctype) <- runFormPost $ renderDivs Acc.newAccountForm
        authLayout $ do
            setTitleI Msg.RegisterLong
            [whamlet|
<div.auth-wrapper>
    ^{apLogin myFacebookPlugin AuthR}
    <i>Or create a separate account below
    <div .newaccountDiv>
       <form method=post enctype=#{enctype} action=@{AuthR Acc.newAccountR}>
           ^{widget}
           <input .btn.btn-full type=submit value=_{Msg.Register}>
    <a .btn.btn-alt.btn-full href=@{AuthR Acc.resetPasswordR}>Forgot password?
|]

    getResetPasswordR = lift $ authLayout $ do
        setTitleI Msg.PasswordResetTitle
        myResetPasswordWidget

-- ** Facebook

myFacebookPlugin :: AuthPlugin App
myFacebookPlugin = defPlugin
    { apLogin = \tm -> [whamlet|
<div.login-facebook>^{apLogin defPlugin tm}
|] } where defPlugin = authFacebook ["email"]

instance YF.YesodFacebook App where
    fbCredentials = appFacebookCredentials . appSettings
    fbHttpManager = getHttpManager

-- * Extra utilities

compileTime :: Text
compileTime = $(runIO getCurrentTime >>= litE . stringL . show)

goGame :: G.InternalEvent -> Handler G.InternalResult
goGame ev = do
    App{..} <- getYesod
    liftIO $ with appGameLock $ do
        putMVar appGameIn ev
        takeMVar appGameOut

-- | Used to higlight navigation links
isNavOf :: Text -> Maybe (Route App) -> Bool
isNavOf "game" (Just LobbyR) = True
isNavOf "game" (Just (PlayR _))  = True

isNavOf "support" (Just SupportR) = True
isNavOf "support" (Just (SupportWithUuidR _)) = True

isNavOf "history" (Just GamesR) = True
isNavOf "history" (Just (ViewR _ _ _ _)) = True

isNavOf "personal" (Just PersonalR) = True

isNavOf _ _ = False
        
userProfilePicture :: User -> Text
userProfilePicture User{..}
    | Just fbId <- userFbUserId = "http://graph.facebook.com/" ++ fbId ++ "/picture?type=square"
    | otherwise                 = "http://www.gravatar.com/avatar/" ++ hashEmail userEmailAddress

-- | for gravatar
hashEmail :: Text -> Text
hashEmail = md5sum . toLower . strip
    where
        md5sum :: Text -> Text
        md5sum = tshow . md5 . C8.pack . unpack

-- | List of text fields.
textListField :: Field Handler [Text]
textListField = Field
    { fieldParse = \xs _ -> return (Right $ Just xs)
    , fieldView  = error "Not viewable"
    , fieldEnctype = UrlEncoded }
