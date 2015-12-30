module Handler.Home where

import Import
import Yesod.Auth.Account
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuthId
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
