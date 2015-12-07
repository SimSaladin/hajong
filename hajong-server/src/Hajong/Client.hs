------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Client
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Client where

------------------------------------------------------------------------------
import           Mahjong
import           Hajong.Connections

------------------------------------------------------------------------------
import           Control.Monad.Logger
import qualified Network.WebSockets as WS

data Client = Client
            { getNick  :: Nick
            , getIdent :: Int -- ^ 0 if none set
            , isReal   :: Bool
            , isReady  :: Bool
            , unicast  :: forall e m. (WS.WebSocketsData e, MonadIO m) => e -> m ()
            , receive  :: forall e m. (WS.WebSocketsData e, MonadIO m) => m e
            }

-- | Client equality is decided by getNick exclusively
instance Eq Client where (==) = (==) `on` getIdent
instance Ord Client where (<=) = (<=) `on` getIdent
-- | show = unpack . getNick
instance Show Client where show = unpack . getNick

instance IsPlayer Client where
    isBot = not . isReal
    playerReady = isReady
    playerNick = getNick

-- * Send

unicastError :: (MonadLogger m, MonadIO m) => Client -> Text -> m ()
unicastError c txt = do
    logErrorN $ "Client error [" ++ getNick c ++ "]: " <> txt
    unicast c $ Invalid txt

clientEither :: (MonadLogger m, MonadIO m) => Client -> Either Text a -> (a -> m ()) -> m ()
clientEither client (Left err) _ = unicastError client err
clientEither _ (Right a) f  = f a

-- * Clients

websocketClient :: Nick -> WS.Connection -> Client
websocketClient nick conn = Client nick 0 True True
    (liftIO . WS.sendTextData conn)
    (liftIO $ WS.receiveData conn)

dummyClient :: Nick -> Client
dummyClient nick = Client nick 0 False False (const (return ())) (error "called receive of dummy Client")
