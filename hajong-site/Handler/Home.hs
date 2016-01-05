module Handler.Home where

import Import

import Handler.SendMail
import qualified Network.Mail.Mime as Mime

import Data.FileEmbed (embedFile)
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getHomeR :: Handler Html
getHomeR = do
    token <- maybe "" id . reqToken <$> getRequest
    muser <- maybeAuthPair
    defaultLayout $ do
        setTitle "Funjong"
        $(widgetFile "homepage")

getSupportR, postSupportR :: Handler Html
getSupportR  = handleSupportWithUuidR Nothing
postSupportR = handleSupportWithUuidR Nothing

getSupportWithUuidR, postSupportWithUuidR :: Text -> Handler Html
getSupportWithUuidR  = handleSupportWithUuidR . Just
postSupportWithUuidR = handleSupportWithUuidR . Just 

handleSupportWithUuidR :: Maybe Text -> Handler Html
handleSupportWithUuidR maybeUuid = do
    museremail <- fmap (userEmailAddress . entityVal . snd) <$> maybeAuthPair
    ((res, widget), enctype) <- runFormPost $ ticketForm museremail maybeUuid

    case res of
        FormSuccess ticket -> do
            tid <- runDB $ insert ticket
            mailTicket (Entity tid ticket) >>= setSession "ticket-body"
            redirect SupportThankYouR
        _ -> return ()

    defaultLayout $ do
        setTitle "Support"
        $(widgetFile "support")

getSupportThankYouR :: Handler Html
getSupportThankYouR = do
    body <- lookupSession "ticket-body" >>= maybe notFound return
    defaultLayout $ do
        setTitle "Thank you"
        $(widgetFile "support-thankyou")

mailTicket :: Entity Ticket -> Handler Text
mailTicket (Entity tid Ticket{..}) = do

    AppSettings{..} <- getsYesod appSettings

    let body = unlines
            [ "Ticket #" ++ tshow (fromSqlKey tid)
            , "Created " ++ tshow ticketCreated
            , "\n" ++ ticketContent ]

        mail = (Mime.emptyMail (Address Nothing ticketEmail))
            { mailBcc     = map (Address Nothing) appSupportTo
            , mailHeaders = [("Subject", "[support] Ticket #" ++ tshow (fromSqlKey tid) ++ " at funjong.org")]
            , mailParts   = [[ Mime.plainPart $ fromStrict body ]] }

    renderSendMail appSupportTo mail
    return body

ticketForm :: Maybe Text -> Maybe Text -> Form Ticket
ticketForm maybeEmail maybeUuid = renderBootstrap3 BootstrapBasicForm $ Ticket
    <$> lift (liftIO getCurrentTime)
    <*> areq emailField "Your email address" maybeEmail
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
    let res = flip map users $ liftA2 (,) userUsername userProfilePicture . entityVal
    returnJson res
