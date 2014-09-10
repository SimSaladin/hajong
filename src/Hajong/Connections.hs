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

--------------------------------------------------------
import Mahjong

(?) :: Monad m => Maybe a -> e -> EitherT e m a
mr ? err = maybe (left err) return mr

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
           | ForceStart Int

           | InGamePrivateEvent GameEvent
           | InGameEvents [GameEvent]
           | InGameAction GameAction
           deriving (Show, Read)

instance ToJSON Event where
    toJSON (JoinServer nick)       = atType "join"         ["nick" .= nick]
    toJSON (PartServer nick)       = atType "part"         ["nick" .= nick]
    toJSON (Message sender cnt)    = atType "msg"          ["from" .= sender, "content" .= cnt]
    toJSON (Invalid msg)           = atType "invalid"      ["content" .= msg]
    toJSON (LoungeInfo lounge)     = atType "lounge"       (loungeJSON lounge)
    toJSON (GameCreated (i,t,n))   = atType "game-created" ["ident" .= i, "topic" .= t, "players" .= n]
    toJSON (JoinGame nth nick)     = atType "game-join"    ["nick" .= nick, "ident" .= nth]
    toJSON (InGamePrivateEvent ge) = atType "game-event"   ["events" .= [gameEventJSON ge]]

gameEventJSON ge = case ge of
    RoundPrivateStarts gameplayer   -> atEvent "round-begin"  (gamePlayerJSON gameplayer)
    RoundPrivateWaitForShout secs   -> atEvent "waiting"      ["time" .= secs]
    RoundPrivateChange kaze hand    -> atEvent "my-hand"      ["player" .= kaze, "hand" .= hand]
    RoundTurnBegins kaze            -> atEvent "turn-changed" []
    RoundTurnAction kaze turnaction -> atEvent "turn-action"  []
    RoundTurnShouted kaze shout     -> atEvent "shout"        []
    RoundHandChanged kaze hand      -> atEvent "hand"         []
    RoundEnded results              -> atEvent "end"          []

instance ToJSON Player where toJSON (Player k) = toJSON k

instance ToJSON Kazehai  where
    toJSON = toJSON . tshow

instance ToJSON Hand where
    toJSON h = object 
        [ "concealed" .= _handConcealed h
        , "pick" .= _handPick h
        , "furiten" .= _handFuriten h
        , "public" .= _handPublic h
        ]

instance ToJSON HandPublic where
    toJSON p = object
        [ "open" .= _handOpen p
        , "discards" .= _handDiscards p
        , "riichi" .= _handRiichi p
        , "turn-discard" .= _handTurnDiscard p
        ]

instance ToJSON Mentsu where toJSON m = undefined
instance ToJSON Tile where toJSON t = undefined

gamePlayerJSON gp = undefined

loungeJSON (Lounge nicks games) = ["idle" .= nicks, "games" .= map gamePairs (M.toList games)]

gamePairs :: (Int, (Text, Set Nick)) -> Value
gamePairs (i,(t,n)) = object ["ident" .= i, "topic" .= t, "players" .= n]

atType, atEvent :: Text -> [(Text, Value)] -> Value
atType t xs = object ("type" .= t : xs)
atEvent t xs = object ("event" .= t : xs)

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
            "game-fstart"  -> ForceStart         <$> o .: "ident"
            "lounge"       -> LoungeInfo         <$> undefined
            _              -> pure (Invalid ("Unknown type: " <> t))
    parseJSON _ = pure (Invalid "Top-level object expected")

-- * Clients

data Client = Client
            { getNick :: Nick
            , isReal  :: Bool
            , isReady :: Bool
            , unicast :: MonadIO m => Event -> m ()
            , receive :: MonadIO m => m Event
            }

instance Eq Client where a == b = getNick a == getNick b
instance Ord Client where a <= b = getNick a <= getNick b
instance Show Client where show = unpack . getNick

dummyClient :: Client
dummyClient = Client "dummy" False False (const (return ())) undefined

-- * Sending to client(s)

multicast :: MonadIO m => GameState Client -> Event -> m ()
multicast gs event = mapM_ (`unicast` event) (gs^.gamePlayers^..each)
    -- TODO Just move this to server already

unicastError :: MonadIO m => Client -> Text -> m ()
unicastError c = unicast c . Invalid

clientEither :: MonadIO m => Client -> Either Text a -> (a -> m ()) -> m ()
clientEither client (Left err) _ = unicastError client err
clientEither _ (Right a) f  = f a

-- * Web socket specific

websocketClient :: Nick -> WS.Connection -> Client
websocketClient nick conn = Client nick True True
    (liftIO . WS.sendTextData conn . encode)
    (liftIO $ fromMaybe (Invalid "No parse") . decode <$> WS.receiveData conn)

instance WS.WebSocketsData Event where
    -- XXX: This should probably be json instead in the future.
    toLazyByteString   = WS.toLazyByteString . tshow
    fromLazyByteString = fromMaybe (Invalid "Malformed event") .  readMay . asText . WS.fromLazyByteString
