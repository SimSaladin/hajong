module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getHomeR :: Handler Html
getHomeR = do
    mauth <- maybeAuth
    defaultLayout $ do
        setTitle "Funjong"
        $(widgetFile "homepage")

getSupportR, postSupportR :: Handler Html
getSupportR  = postSupportR
postSupportR = do
    ((res, widget), enctype) <- runFormPost ticketForm

    submitted <- case res of
        FormSuccess ticket -> do
            tid <- runDB $ insert ticket
            return (Just tid)
        _ -> return Nothing

    defaultLayout $ do
        setTitle "Support"
        $(widgetFile "support")

ticketForm :: Form Ticket
ticketForm = renderBootstrap3 BootstrapBasicForm $ Ticket
    <$> areq emailField "Your email address" Nothing
    <*> fmap unTextarea (areq textareaField "Description" Nothing)
