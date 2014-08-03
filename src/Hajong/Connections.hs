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

import           ClassyPrelude
import           Control.Monad.Trans.Either
import           Control.Monad.Reader.Class
import           Control.Lens
import qualified Network.WebSockets         as WS

import Hajong.Game

type Nick = Text

data Lounge = Lounge
            { _loungeNicksIdle :: Set Nick
            , _loungeGames :: Map Int (Text, Set Nick)
            } deriving (Show, Read)

makeLenses ''Lounge

data Event = JoinServer Text -- ^ Nick
           | PartServer Text
           | LoungeInfo Lounge
           | Message Text Text -- ^ from, content
           | Invalid Text
           -- Game events
           | CreateGame Text
           | NewGame (Int, Text, Set Nick) -- name, nicks
           | RoundStarts (GamePlayer Nick)
           | JoinGame Int Text -- ^ Game lounge
           -- In-game events
           | GameAction TurnAction -- ^ From client to server only
           | GameDontCare -- ^ About discarded tile
           | GameEvents [RoundEvent]
           | GameHandChanged Hand -- Own hand only
           | GameShout Shout
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
multicast gs event = forM_ (gs^.gamePlayers^..(each._2._Just)) (`unicast` event)

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

-- * General

rview :: (MonadReader s m, MonadIO m) => Getting (MVar b) s (MVar b) -> m b
rview l = view l >>= liftIO . readMVar

rswap :: (MonadReader s m, MonadIO m) => Getting (MVar b) s (MVar b) -> b -> m b 
rswap l a = view l >>= liftIO . (`swapMVar` a)

rmodify :: (MonadReader s m, MonadIO m) => Getting (MVar a) s (MVar a) -> (a -> IO a) -> m ()
rmodify l f = view l >>= liftIO . (`modifyMVar_` f)
