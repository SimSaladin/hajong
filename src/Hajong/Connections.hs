{-# OPTIONS_GHC -fno-warn-orphans #-}
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

------------------------------------------------------------------------------
import           Mahjong

------------------------------------------------------------------------------
import           Prelude hiding ((.=))
import qualified Data.Map as M
import           Control.Monad.Trans.Either
import qualified Network.WebSockets         as WS
import           Data.Aeson
import           Data.Aeson.Types (Pair)

------------------------------------------------------------------------------

(?) :: Monad m => Maybe a -> e -> EitherT e m a
mr ? err = maybe (left err) return mr

-- * Event

data Event = JoinServer Nick
           | PartServer Nick
           | ClientIdentity Nick
           | Message Nick Text -- ^ from, content
           | Invalid Text

           | LoungeInfo Lounge
           | GameCreated (Int, Text, Set Nick) -- id, name, nicks
           | CreateGame Text
           | JoinGame Int Nick -- ^ game num, nick
           | ForceStart Int

           | InGamePrivateEvent GameEvent
           | InGameEvents [GameEvent]
           | InGameAction GameAction
           deriving (Show, Read)

-- * Clients

type Nick = Text

data Lounge = Lounge
            { _loungeNicksIdle :: Set Nick
            , _loungeGames :: Map Int (Text, Set Nick)
            } deriving (Show, Read)

makeLenses ''Lounge

-- | NOTE: Client equality is decided by getNick exclusively, and show
-- = unpack . getNick.
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

instance IsPlayer Client where
    isBot = not . isReal
    playerReady = isReady
    playerNick = getNick

dummyClient :: Nick -> Client
dummyClient nick = Client nick False False (const (return ())) (error "called receive of dummy Client")

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
    (liftIO . WS.sendTextData conn)
    (liftIO $ WS.receiveData conn)

instance WS.WebSocketsData Event where
    toLazyByteString   = encode
    fromLazyByteString = fromMaybe (Invalid "Malformed event") . decode

-- ToJSON boilerplate - TODO derive us instead? ------------------------------

-- Helpers -------------------------------------------------------------------

atType, atEvent :: Text -> [(Text, Value)] -> Value
atType  t xs = object ("type"  .= t : xs)
atEvent t xs = object ("event" .= t : xs)

gamePlayerJSON :: GamePlayer -> [Pair]
gamePlayerJSON x =
    [ "kaze"      .= _playerKaze x
    , "player"    .= _playerPlayer x
    , "hands"     .= map (toJSON *** toJSON) (M.toList $ _playerPublicHands x)
    , "myhand"    .= _playerMyHand x
    , "gamestate" .= _playerPublic x
    ]

loungeJSON :: Lounge -> [Pair]
loungeJSON (Lounge nicks games) = ["idle" .= nicks, "games" .= map gamePairs (M.toList games)]

gamePairs :: (Int, (Text, Set Nick)) -> Value
gamePairs (i,(t,n)) = object [ "ident"   .= i
                             , "topic"   .= t
                             , "players" .= n
                             ]

-- Instances -----------------------------------------------------------------

instance ToJSON Event where
    toJSON (ClientIdentity nick)   = atType "identity"     ["nick" .= nick]
    toJSON (JoinServer nick)       = atType "join"         ["nick" .= nick]
    toJSON (PartServer nick)       = atType "part"         ["nick" .= nick]
    toJSON (Message sender cnt)    = atType "msg"          ["from" .= sender, "content" .= cnt]
    toJSON (Invalid msg)           = atType "invalid"      ["content" .= msg]
    toJSON (LoungeInfo lounge)     = atType "lounge"       (loungeJSON lounge)
    toJSON (GameCreated (i,t,n))   = atType "game-created" ["ident" .= i, "topic" .= t, "players" .= n]
    toJSON (JoinGame nth nick)     = atType "game-join"    ["nick" .= nick, "ident" .= nth]
    toJSON (InGamePrivateEvent x)  = atType "game-event"   ["events" .= [x] ]
    toJSON (InGameEvents xs)       = atType "game-event"   ["events" .= xs  ]
    toJSON x                       = error $ "toJSON called for an Event that shouldn't be deserialized on server: " ++ show x

instance ToJSON GameEvent where
    toJSON ge = case ge of
        RoundPrivateStarts gameplayer           -> atEvent "round-begin"  (gamePlayerJSON gameplayer)
        RoundPrivateWaitForShout player secs      -> atEvent "wait-shout"   ["player" .= player, "seconds" .= secs]
        RoundPrivateWaitForTurnAction player secs -> atEvent "wait-turn"    ["player" .= player, "seconds" .= secs]
        RoundPrivateChange player hand            -> atEvent "my-hand"      ["player" .= player, "hand" .= hand]
        RoundTurnBegins pk                    -> atEvent "turn-changed" ["player-kaze" .= pk]
        RoundTurnAction pk turnaction         -> atEvent "turn-action"  ["player-kaze" .= pk, "action" .= turnaction]
        RoundTurnShouted pk shout             -> atEvent "shout"        ["player-kaze" .= pk, "shout" .= shout]
        RoundHandChanged pk hand              -> atEvent "hand"         ["player-kaze" .= pk, "hand" .= hand]
        RoundEnded results                      -> atEvent "end"          ["results" .= results]

instance ToJSON TurnAction where
    toJSON (TurnTileDiscard r t) = atType "discard" ["riichi" .= r, "tile" .= t]
    toJSON (TurnTileDraw w mt)   = atType "draw" ["wanpai" .= w, "tile" .= mt]
    toJSON (TurnAnkan t)         = atType "ankan" ["tile" .= t]

instance ToJSON Player where
    toJSON (Player k) = toJSON k

instance ToJSON Kaze where
    toJSON = toJSON . tshow

instance ToJSON Hand where
    toJSON h = Object $
        (\(Object o) -> o) (object
            [ "concealed" .= _handConcealed h
            , "pick"      .= _handPick h
            , "furiten"   .= _handFuriten h
            ])
        <> (\(Object o) -> o) (toJSON (_handPublic h))

instance ToJSON HandPublic where
    toJSON p = object
        [ "called"       .= _handOpen p
        , "discards"     .= _handDiscards p
        , "riichi"       .= _handRiichi p
        , "turn-discard" .= _handTurnDiscard p
        ]

instance ToJSON Mentsu where
    toJSON (Mentsu mk t ms) = object [ "type" .= mk, "tile" .= t, "shouted" .= ms ]

instance ToJSON MentsuKind where toJSON = toJSON . tshow

instance ToJSON Shout where
    toJSON (Pon p tile) = atType "pon"    ["from" .= p, "tile" .= tile, "to" .= ([] :: [Tile])]
    toJSON (Kan p tile) = atType "kan"    ["from" .= p, "tile" .= tile, "to" .= ([] :: [Tile])]
    toJSON (Chi p tile xs) = atType "chi" ["from" .= p, "tile" .= tile, "to" .= xs]
    toJSON (Ron p tile xs) = atType "ron" ["from" .= p, "tile" .= tile, "to" .= xs]

instance ToJSON Tile where
    toJSON (Suited tk n a) = object [ "type" .= tk, "number" .= n, "aka" .= a ]
    toJSON (Honor h) = object [ "type" .= HonorTile, "ident" .= h]

instance ToJSON Number where toJSON = toJSON . (1 +) . fromEnum
instance ToJSON TileKind where toJSON = toJSON . tshow

instance ToJSON Honor where
    toJSON (Sangenpai s) = toJSON (tshow s)
    toJSON (Kazehai k) = toJSON k

instance ToJSON RoundResults where
    toJSON res = atType getType ["winners" .= winners res, "payers" .= payers res]
        where
            getType = case res of RoundTsumo{} -> "tsumo"
                                  RoundRon{}   -> "ron"
                                  RoundDraw{}  -> "draw"

instance ToJSON RiichiPublic where
    toJSON x = object
        [ "dora"       .= _riichiDora x
        , "tiles-left" .= _riichiWallTilesLeft x
        , "round"      .= _riichiRound x
        , "oja"        .= _riichiOja x
        , "first-oja"  .= _riichiFirstOja x
        , "turn"       .= _riichiTurn x
        , "points"     .= ((\(a,_,c) -> (a,c)) <$> _riichiPlayers x)
        , "players"    .= ((\(a,Player b,_) -> (a,b)) <$> _riichiPlayers x)
        , "results"    .= _riichiResults x
        ]

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
            "game-action"  -> InGameAction       <$> gameActionFromJSON o
            "game-create"  -> CreateGame         <$> o .: "topic"
            "game-fstart"  -> ForceStart         <$> o .: "ident"
            _              -> pure (Invalid ("Unknown or unsupported type: " <> t))
    parseJSON _ = pure (Invalid "Top-level object expected")


gameActionFromJSON o = do
    t <- o .: "action"
    case t :: Text of
        "discard" -> (\x y -> GameTurn $ TurnTileDiscard x y) <$> o .: "riichi" <*> o .: "tile"
        "draw"    -> (\x y -> GameTurn $ TurnTileDraw x y) <$> o .: "dead" <*> pure Nothing
        "ankan"   -> GameTurn . TurnAnkan <$> o .: "tile"
        "pass"    -> pure GameDontCare
        "shout"   -> do s <- o .: "shout"
                        GameShout <$> case s :: Text of
                            "pon" -> Pon <$> o .: "from" <*> o .: "tile"
                            "kan" -> Kan <$> o .: "from" <*> o .: "tile"
                            "chi" -> Chi <$> o .: "from" <*> o .: "tile" <*> o .: "into"
                            "ron" -> Ron <$> o .: "from" <*> o .: "tile" <*> o .: "into"

instance FromJSON Number where
    parseJSON = fmap (toEnum . (\x -> x - 1)) . parseJSON

instance FromJSON Player where
    parseJSON = fmap Player . parseJSON

instance FromJSON Tile where
    parseJSON (Object o) = do
        t <- o .: "type"
        case t :: Text of
            "ManTile"   -> Suited ManTile <$> o .: "number" <*> o .: "aka"
            "PinTile"   -> Suited PinTile <$> o .: "number" <*> o .: "aka"
            "SouTile"   -> Suited SouTile <$> o .: "number" <*> o .: "aka"
            "HonorTile" -> Honor <$> o .: "ident"

instance FromJSON Honor where
    parseJSON (String s) = pure $ case s of
                               "Haku"  -> Sangenpai Haku
                               "Hatsu" -> Sangenpai Hatsu
                               "Chun"  -> Sangenpai Chun
                               "Ton"  -> Kazehai Ton
                               "Nan"  -> Kazehai Nan
                               "Shaa" -> Kazehai Shaa
                               "Pei"  -> Kazehai Pei

instance FromJSON Kaze where
    parseJSON (String s) = pure $ case s of
                               "Ton"  -> Ton
                               "Nan"  -> Nan
                               "Shaa" -> Shaa
                               "Pei"  -> Pei
