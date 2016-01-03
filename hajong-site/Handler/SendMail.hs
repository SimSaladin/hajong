------------------------------------------------------------------------------
-- | 
-- Module         : Handler.SendMail
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2016-01-02T17:58:05+0200
------------------------------------------------------------------------------
module Handler.SendMail
    ( module Handler.SendMail
    , SES.SES
    ) where

import ClassyPrelude.Yesod
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))

import qualified Network.Mail.Mime     as Mime
import qualified Network.Mail.Mime.SES as SES

class (Yesod master, HasHttpManager master) => YesodSES master where
    getSES :: YesodSES master => HandlerT master IO SES.SES

instance FromJSON SES.SES where
    parseJSON = withObject "SES" $ \o -> do
        sesFrom      <- encodeUtf8 . asText <$> o .: "ses_from"
        sesAccessKey <- encodeUtf8 . asText <$> o .: "access_key"
        sesSecretKey <- encodeUtf8 . asText <$> o .: "secret_key"
        sesRegion    <- o .: "region"
        return SES.SES{..}

sendMail :: YesodSES master => [Text] -> LByteString -> HandlerT master IO ()
sendMail to bs = do
    mgr <- getsYesod getHttpManager
    ses <- getSES
    SES.sendMailSES mgr ses { SES.sesTo = map encodeUtf8 to } bs

renderSendMail :: YesodSES master => [Text] -> Mime.Mail -> HandlerT master IO ()
renderSendMail to mail = do
    mgr <- getsYesod getHttpManager
    ses <- getSES
    SES.renderSendMailSES mgr ses { SES.sesTo = map encodeUtf8 to } mail
