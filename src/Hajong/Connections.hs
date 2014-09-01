{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Connections
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Data types of information exchanged between server and clients.
------------------------------------------------------------------------------
module Hajong.Connections where

import           Prelude hiding ((.=))
import qualified Data.Map as M
import           Control.Monad.Trans.Either
import qualified Network.WebSockets         as WS
import           Data.Aeson
import Hajong.Game

-- * Functions

{-# DEPRECATED maybeToEitherT "use ? instead" #-}

-- | convert maybe to EitherT
maybeToEitherT :: Monad m => e -> Maybe a -> EitherT e m a
maybeToEitherT def = maybe (left def) return

(?) :: Monad m => Maybe a -> e -> EitherT e m a
(?) = flip maybeToEitherT

-- * Events 'n stuff

type Nick = Text

data Lounge = Lounge
            { _loungeNicksIdle :: Set Nick
            , _loungeGames :: Map Int (Text, Set Nick)
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

instance ToJSON Event where
    toJSON (JoinServer nick)     = atType "join" ["nick" .= nick]
    toJSON (PartServer nick)     = atType "part" ["nick" .= nick]
    toJSON (Message sender cnt)  = atType "msg" ["from" .= sender, "content" .= cnt]
    toJSON (Invalid msg)         = atType "invalid" ["content" .= msg]
    toJSON (LoungeInfo lounge)   = atType "lounge" (loungeJSON lounge)
    toJSON (GameCreated (i,t,n)) = atType "game-created" ["ident" .= i, "topic" .= t, "players" .= n]
    toJSON (JoinGame nth nick)   = atType "gaem-join" ["nick" .= nick, "ident" .= nth]

loungeJSON (Lounge nicks games) = ["idle" .= nicks, "games" .= map gamePairs (M.toList games)]

gamePairs :: (Int, (Text, Set Nick)) -> Value
gamePairs (i,(t,n)) = object ["ident" .= i, "topic" .= t, "players" .= n]

atType :: Text -> [(Text, Value)] -> Value
atType t xs = object ("type" .= t : xs)

instance FromJSON Event where
    parseJSON (Object o) = do
        t <- o .: "type"
        case t :: Text of
            "join"         -> JoinServer         <$> o .: "nick"
            "part"         -> PartServer         <$> o .: "nick"
            "msg"          -> Message            <$> o .: "from" <*> o .: "content"
            "game-created" -> (\x y z -> GameCreated (x,y,z)) <$> o .: "ident" <*> o .: "topic" <*> o .: "players"
            "game-join"    -> JoinGame           <$> o .: "ident" <*> o .: "nick"
            "game-secret"  -> InGamePrivateEvent <$> undefined
            "game-public"  -> InGameEvents       <$> undefined
            "game-action"  -> InGameAction       <$> undefined
            "game-create"  -> CreateGame         <$> o .: "topic"
            "lounge"       -> LoungeInfo         <$> undefined
            _              -> pure (Invalid ("Unknown type: " <> t))
    parseJSON _ = pure (Invalid "Top-level object expected")

-- * Clients

data Client = Client
            { getNick :: Nick
            , unicast :: MonadIO m => Event -> m ()
            , receive :: MonadIO m => m Event
            }

instance Eq Client where a == b = getNick a == getNick b
instance Ord Client where a <= b = getNick a <= getNick b
instance Show Client where show = unpack . getNick

-- * Sending to client(s)

multicast :: MonadIO m => GameState Client -> Event -> m ()
multicast gs event = mapM_ (`unicast` event) (gs^.gamePlayers^..each._Just)
    -- TODO Just move this to server already

unicastError :: MonadIO m => Client -> Text -> m ()
unicastError c = unicast c . Invalid

clientEither :: MonadIO m => Client -> Either Text a -> (a -> m ()) -> m ()
clientEither client (Left err) _ = unicastError client err
clientEither _ (Right a) f  = f a

-- * Web socket specific

websocketClient :: Nick -> WS.Connection -> Client
websocketClient nick conn = Client nick
    (liftIO . WS.sendTextData conn . encode)
    (liftIO $ fromMaybe (Invalid "No parse") . decode <$> WS.receiveData conn)

instance WS.WebSocketsData Event where
    -- XXX: This should probably be json instead in the future.
    toLazyByteString   = WS.toLazyByteString . tshow
    fromLazyByteString = fromMaybe (Invalid "Malformed event") .  readMay . asText . WS.fromLazyByteString
