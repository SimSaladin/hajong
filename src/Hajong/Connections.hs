{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Connections
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
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
import           Control.Monad.Logger
import qualified Network.WebSockets         as WS
import           Data.Aeson hiding (Value)
import qualified Data.Aeson as A
import           Data.Aeson.TH
import           Data.Aeson.Types (Pair)

------------------------------------------------------------------------------

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

data GameSettings = GameSettings { gameTitle :: Text }
                  deriving (Show, Read, Typeable)

data Lounge = Lounge
            { _loungeNicksIdle :: Set Nick
            , _loungeGames :: Map Int GameSettings
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

unicastError :: (MonadLogger m, MonadIO m) => Client -> Text -> m ()
unicastError c txt = do
    $logError $ "Client error [" ++ getNick c ++ "]: " <> txt
    unicast c $ Invalid txt

clientEither :: (MonadLogger m, MonadIO m) => Client -> Either Text a -> (a -> m ()) -> m ()
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

-- Helpers -------------------------------------------------------------------

atType, atEvent :: Text -> [(Text, A.Value)] -> A.Value
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

gamePairs :: (Int, GameSettings) -> A.Value
gamePairs (i,GameSettings t) = object
    [ "ident"   .= i , "topic"   .= t , "players" .= (mempty :: Set Text) ]
        -- TODO players, necessary?

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
        DealPrivateStarts gameplayer             -> atEvent "round-begin"  (gamePlayerJSON gameplayer)
        DealPrivateWaitForShout player secs shs  -> atEvent "wait-shout"   ["player" .= player, "seconds" .= secs, "shouts" .= shs]
        DealPrivateWaitForTurnAction p sec rs    -> atEvent "wait-turn"    ["player" .= p, "seconds" .= sec, "riichi-with" .= rs]
        DealPrivateChange player hand            -> atEvent "my-hand"      ["player" .= player, "hand" .= hand]
        DealTurnBegins pk                        -> atEvent "turn-changed" ["player-kaze" .= pk]
        DealTurnAction pk turnaction             -> atEvent "turn-action"  ["player-kaze" .= pk, "action" .= turnaction]
        DealTurnShouted pk shout                 -> atEvent "shout"        ["player-kaze" .= pk, "shout" .= shout]
        DealHandChanged pk hand                  -> atEvent "hand"         ["player-kaze" .= pk, "hand" .= hand]
        DealEnded results                        -> atEvent "end"          ["results" .= results]
        DealNick p pk nick                       -> atEvent "nick"         ["player" .= p, "player-kaze" .= pk, "nick" .= nick]

instance ToJSON TurnAction where
    toJSON (TurnTileDiscard r t) = atType "discard"    ["riichi" .= r, "tile" .= t]
    toJSON (TurnTileDraw w mt)   = atType "draw"       ["wanpai" .= w, "tile" .= mt]
    toJSON (TurnAnkan t)         = atType "ankan"      ["tile" .= t]
    toJSON (TurnShouminkan t)    = atType "shouminkan" ["tile" .= t]
    toJSON TurnTsumo             = atType "tsumo"      []

instance ToJSON Player where toJSON (Player k) = toJSON k
instance ToJSON Kaze where toJSON = toJSON . tshow
instance ToJSON Number where toJSON = toJSON . (1 +) . fromEnum
instance ToJSON TileKind where toJSON = toJSON . tshow
instance ToJSON Hand where
    toJSON h = Object $
        (\(Object o) -> o) (object
            [ "concealed" .= _handConcealed h
            , "pick"      .= _handPick h
            , "furiten"   .= _handFuriten h
            ])
        <> (\(Object o) -> o) (toJSON (_handPublic h))

instance ToJSON Tile where
    toJSON (Suited tk n a) = object [ "type" .= tk, "number" .= n, "aka" .= a ]
    toJSON (Honor h) = object [ "type" .= HonorTile, "ident" .= h]

instance ToJSON Honor where
    toJSON (Sangenpai s) = toJSON (tshow s)
    toJSON (Kazehai k) = toJSON k

instance ToJSON Deal where
    toJSON x = object
        [ "dora"       .= _pDora x
        , "tiles-left" .= _pWallTilesLeft x
        , "round"      .= _pRound x
        , "oja"        .= _pOja x
        , "first-oja"  .= _pFirstOja x
        , "turn"       .= _pTurn x
        , "players"    .= map (toJSON *** toJSON) (M.toList $ _pPlayers x)
        , "results"    .= _pResults x
        ]

-- derived

--
$(deriveToJSON (aesonOptions 5) ''HandPublic)
$(deriveToJSON (aesonOptions 6) ''Mentsu)
$(deriveToJSON (aesonOptions 0) ''MentsuKind)
$(deriveToJSON (aesonOptions 0) ''ShoutKind)
$(deriveToJSON (aesonOptions 5) ''Shout)
$(deriveToJSON (aesonOptions' 1 4) ''DealResults)
$(deriveToJSON (aesonOptions 4) ''Yaku)
$(deriveToJSON (aesonOptions 3) ''Value)
$(deriveToJSON (aesonOptions 3) ''ValuedHand)
$(deriveToJSON (aesonOptions 0) ''AbortiveDraw)

-- FromJSON

instance FromJSON Event where
    parseJSON v@(Object o) = do
        t <- o .: "type"
        case t :: Text of
            "join"         -> JoinServer         <$> o .: "nick"
            "part"         -> PartServer         <$> o .: "nick"
            "msg"          -> Message            <$> o .: "from" <*> o .: "content"
            "game-created" -> (\x y z -> GameCreated (x,y,z)) <$> o .: "ident" <*> o .: "topic" <*> o .: "players"
            "game-join"    -> JoinGame           <$> o .: "ident" <*> o .: "nick"
            "game-secret"  -> fail "From server only event" -- InGamePrivateEvent <$> undefined
            "game-public"  -> fail "From server only event" -- InGameEvents       <$> undefined
            "game-action"  -> InGameAction       <$> parseJSON v
            "game-create"  -> CreateGame         <$> o .: "topic"
            "game-fstart"  -> ForceStart         <$> o .: "ident"
            _              -> pure (Invalid ("Unknown or unsupported type: " <> t))
    parseJSON _ = pure (Invalid "Top-level object expected")

instance FromJSON GameAction where
    parseJSON v@(Object o)
        | Success ta <- fromJSON v = return $ GameTurn ta
        | otherwise = do
            t <- o .: "action"
            case t :: Text of
                "pass"    -> pure GameDontCare
                "shout"   -> GameShout <$> parseJSON v
                _         -> fail "Game 'action' not recognized"
    parseJSON _ = fail "Expected an object"
                
instance FromJSON Shout where
    parseJSON (Object o) = Shout <$> o .: "shout"
                                 <*> o .: "from"
                                 <*> o .: "tile"
                                 <*> o .: "into"
    parseJSON _ = fail "Expected an object"

instance FromJSON ShoutKind where
    parseJSON (String s) = case s of
        "pon" -> return Pon
        "kan" -> return Kan
        "chi" -> return Chi
        "ron" -> return Ron
        _     -> fail "shout no parse"
    parseJSON _ = fail "Expected a string"

instance FromJSON TurnAction where
    parseJSON (Object o) = do
        t <- o .: "action"
        case t :: Text of
            "discard" -> TurnTileDiscard <$> o .: "riichi" <*> o .: "tile"
            "draw"    -> TurnTileDraw    <$> o .: "dead"   <*> pure Nothing
            "ankan"   -> TurnAnkan       <$> o .: "tile"
            _         -> fail "TurnAction type not recognized"
    parseJSON _ = fail "Expected an object"

instance FromJSON Number where
    parseJSON v = do
        n <- parseJSON v <&> id -~ 1
        if n <= fromEnum (maxBound :: Number) && n >= fromEnum (minBound :: Number)
            then pure (toEnum n)
            else fail "Tile number out of bounds"

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
            _ -> fail "Tile type not recognized"
    parseJSON _ = fail "Expected an object"

instance FromJSON Honor where
    parseJSON (String s) = case s of
        "Haku"  -> pure $ Sangenpai Haku
        "Hatsu" -> pure $ Sangenpai Hatsu
        "Chun"  -> pure $ Sangenpai Chun
        "Ton"   -> pure $ Kazehai Ton
        "Nan"   -> pure $ Kazehai Nan
        "Shaa"  -> pure $ Kazehai Shaa
        "Pei"   -> pure $ Kazehai Pei
        _       -> fail "Honor not recognized"
    parseJSON _ = fail "Expected a string"

instance FromJSON Kaze where
    parseJSON (String s) = case s of
        "Ton"  -> pure Ton
        "Nan"  -> pure Nan
        "Shaa" -> pure Shaa
        "Pei"  -> pure Pei
        _      -> fail "Kaze not recognized"
    parseJSON _ = fail "Expected a string"
