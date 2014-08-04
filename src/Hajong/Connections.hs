{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Connections
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Connections where

import           Control.Monad.Trans.Either
import qualified Network.WebSockets         as WS

import Hajong.Game

type Nick = Text

data Lounge = Lounge
            { _loungeNicksIdle :: Set Nick
            , _loungeGames :: Map Int Text -- TODO (Text, Set Nick)
            } deriving (Show, Read)

makeLenses ''Lounge

data Event = JoinServer Text -- ^ Nick
           | PartServer Text
           | Message Text Text -- ^ from, content
           | Invalid Text

           | LoungeInfo Lounge
           | GameCreated (Int, Text, Set Nick) -- name, nicks
           | CreateGame Text
           | JoinGame Int Text -- ^ Game lounge

           | InGamePrivateEvent GameEvent
           | InGameEvents [GameEvent]
           | InGameAction GameAction
           deriving (Show, Read)

-- * Clients

data Client = Client
            { getNick :: Nick
            , unicast :: MonadIO m => Event -> m ()
            , receive :: MonadIO m => m Event
            }

instance Eq Client where a == b = getNick a == getNick b
instance Ord Client where a <= b = getNick a <= getNick b
instance Show Client where show = unpack . getNick

multicast :: MonadIO m => GameState Client -> Event -> m ()
multicast gs event = mapM_ (`unicast` event) (gs^.gamePlayers^..each._Just)

unicastError :: MonadIO m => Client -> Text -> m ()
unicastError c = unicast c . Invalid

clientEither :: MonadIO m => Client -> Either Text a -> (a -> m ()) -> m ()
clientEither client (Left err) _ = unicastError client err
clientEither _ (Right a) f  = f a

-- * Websocket clients

websocketClient :: Nick -> WS.Connection -> Client
websocketClient nick conn = Client nick (liftIO . WS.sendTextData conn) (liftIO $ WS.receiveData conn)

instance WS.WebSocketsData Event where
    toLazyByteString   = WS.toLazyByteString . tshow
    fromLazyByteString = fromMaybe (Invalid "Malformed event") .  readMay . asText . WS.fromLazyByteString

-- | convert maybe to EitherT
maybeToEitherT :: Monad m => e -> Maybe a -> EitherT e m a
maybeToEitherT def = maybe (left def) return
