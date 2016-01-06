{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Handler.SendMail
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- It's useful to import this with @Network.Mail.Mime@. We re-export @Mime@
-- and @Address@ for convenience
--
-- @
-- import Handler.SendMail
-- import qualified Network.Mail.Mime as Mime
-- @
------------------------------------------------------------------------------
module Handler.SendMail
    ( module Handler.SendMail
    , SES.SES
    , Mime.Mail(..), Mime.Address(..)
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
        sesFrom      <- encodeUtf8 . asText <$> o .: "from"
        sesAccessKey <- encodeUtf8 . asText <$> o .: "access-key"
        sesSecretKey <- encodeUtf8 . asText <$> o .: "secret-key"
        sesRegion    <- o .: "region"
        sesTo        <- map (encodeUtf8 . asText) <$> o .:? "to" .!= []
        return SES.SES{..}

renderSendMail :: YesodSES master => [Text] -> Mime.Mail -> HandlerT master IO ()
renderSendMail to mail = do
    let mail' = mail { Mime.mailFrom = appInfoAddress }
    mgr <- getsYesod getHttpManager
    ses <- getSES
    $(logInfo) ("mail to <" ++ tshow to ++ ">: " ++ tshow mail')
#if !DEVELOPMENT
    SES.renderSendMailSES mgr ses { SES.sesTo = SES.sesTo ses ++ map encodeUtf8 to } mail'
#endif

-- | TODO into config
appInfoAddress :: Mime.Address
appInfoAddress = Mime.Address Nothing "info@funjong.org"
