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
import           Control.Monad.Trans.Control
import qualified Network.WebSockets as WS

data Client = Client
            { getNick  :: Nick
            , getIdent :: Int  -- ^ 0 if none set
            , isReal   :: Bool -- ^ False for bots server-side
            , isReady  :: Bool -- ^ Ready to start a game(?)
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

-- | Won't raise errors e.g. on sudden client disconnects.
unicastError :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Client -> Text -> m ()
unicastError c txt = do
    logErrorN $ "Client error [" ++ getNick c ++ "]: " <> txt
    safeUnicast c $ Invalid txt

-- | Won't raise errors e.g. on sudden client disconnects.
safeUnicast :: (MonadBaseControl IO m, Show e, WS.WebSocketsData e, MonadLogger m, MonadIO m) => Client -> e -> m ()
safeUnicast c e = handle (logSomeException $ "sending " ++ tshow e ++ "to client " ++ tshow c ++ "failed: ")
    $ unicast c e

safeReceive :: (MonadBaseControl IO m, WS.WebSocketsData e, MonadLogger m, MonadIO m) => Client -> m (Maybe e)
safeReceive c = handle (\e -> logSomeException ("receive from client " ++ tshow c ++ "falied: ") e >> return Nothing)
    $ fmap Just $ receive c

logSomeException :: MonadLogger m => Text -> SomeException -> m ()
logSomeException tmpl e = logErrorN $ tmpl ++ tshow e

-- | @clientEither c r f@: if r fails, notify client. otherwise return f of
-- the result.
clientEither :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Client -> Either Text a -> (a -> m ()) -> m ()
clientEither client (Left err) _ = unicastError client err
clientEither _ (Right a) f  = f a

-- * Clients

websocketClient :: Nick -> WS.Connection -> Client
websocketClient nick conn = Client nick 0 True True
    (liftIO . WS.sendTextData conn)
    (liftIO $ WS.receiveData conn)

dummyClient :: Nick -> Client
dummyClient nick = Client nick 0 False False (const (return ())) (error "called receive of dummy Client")
