module Handler.Home where

import Import
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text (strip)
import Data.Digest.Pure.MD5 (md5)
import Data.FileEmbed (embedFile)
import Yesod.Auth.Account
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuthPair
    defaultLayout $ do
        setTitle "Funjong"
        $(widgetFile "homepage")

getSupportR, postSupportR :: Handler Html
getSupportR  = postSupportR
postSupportR = supportWithUuidR Nothing

supportWithUuidR :: Maybe Text -> Handler Html
supportWithUuidR maybeUuid = do
    ((res, widget), enctype) <- runFormPost $ ticketForm maybeUuid

    submitted <- case res of
        FormSuccess ticket -> do
            tid <- runDB $ insert ticket
            return (Just tid)
        _ -> return Nothing

    -- TODO: send email to me
    defaultLayout $ do
        setTitle "Support"
        $(widgetFile "support")

getSupportWithUuidR, postSupportWithUuidR :: Text -> Handler Html
getSupportWithUuidR  = postSupportWithUuidR
postSupportWithUuidR = supportWithUuidR . Just 

ticketForm :: Maybe Text -> Form Ticket
ticketForm maybeUuid = renderBootstrap3 BootstrapBasicForm $ Ticket
    <$> lift (liftIO getCurrentTime)
    <*> areq emailField "Your email address" Nothing
    <*> fmap unTextarea (areq textareaField "Description" { fsAttrs = [("rows", "17")] } $ Just $ ticketTemplate maybeUuid)

ticketTemplate :: Maybe Text -> Textarea
ticketTemplate uuid = Textarea $ unlines
    [ "This report is related to a game: " ++ maybe "(none)" ("game identifier: " ++) uuid
    , ""
    , "What you did?\n"
    , "What you expected to happen?\n"
    , "What happened instead?\n"
    ]


-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getProfilePicturesR :: Handler Value
getProfilePicturesR = do
    nicks <- runInputGet $ ireq textListField "nicks[]"
    users <- runDB $ selectList [UserUsername <-. nicks] []

    let res = flip map users $ \(Entity _ User{..}) ->
            let profpic | Just fbId <- userFbUserId = "http://graph.facebook.com/" ++ fbId ++ "/picture?type=square"
                        | otherwise                 = "http://www.gravatar.com/avatar/" ++ hashEmail userEmailAddress
                in (userUsername, profpic)

    returnJson res

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
