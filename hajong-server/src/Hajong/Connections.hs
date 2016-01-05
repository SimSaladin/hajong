{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Identity/Maybe paramaterization
{-# LANGUAGE DeriveGeneric #-}
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
import           Mahjong hiding ((.=))

------------------------------------------------------------------------------
import qualified Data.Map as M
import qualified Network.WebSockets         as WS
import           Data.Aeson hiding (Value)
import qualified Data.Aeson as A
import           Data.Aeson.TH
import           Data.Aeson.Types (Pair)
import qualified Data.Binary as B
import           Data.Text.Binary ()
import qualified Data.UUID    as UUID

------------------------------------------------------------------------------

instance ToJSON m => ToJSON (Identity m) where
    toJSON (Identity x) = toJSON x

type Nick = Text

-- * Event

data Event = JoinServer Nick Int Text (Maybe Int) -- nick, ident, token, game to join
           | PartServer Nick
           | ClientIdentity Nick Int Text
           | Message Nick Text -- ^ from, content
           | Invalid Text

           | LoungeInfo Lounge
           | GameCreated (Int, Text, [Nick], Text) -- id, name, nicks, uuid
           | JoinGame Int Nick -- ^ game num, nick
           | PartGame Nick
           | ForceStart Int

           | InGamePrivateEvent GameEvent
           | InGameEvents [GameEvent]
           | InGameAction GameAction

           | InternalControl Text
           deriving (Show, Read)

-- | Uses the aeson instances
instance WS.WebSocketsData Event where
    toLazyByteString   = encode
    fromLazyByteString = either (Invalid . ("Malformed event: " ++) . pack) id . eitherDecode

data InternalEvent = InternalNewGame GameSettings
                   deriving (Show, Read, Generic)

data InternalResult = InternalGameCreated Int
                    | InternalError Text
                    deriving (Show, Read, Generic)

-- | Uses Binary
instance WS.WebSocketsData InternalEvent where
    toLazyByteString   = B.encode
    fromLazyByteString = B.decode

-- | Uses Binary
instance WS.WebSocketsData InternalResult where
    toLazyByteString   = B.encode
    fromLazyByteString = B.decode

instance B.Binary InternalEvent
instance B.Binary InternalResult
instance B.Binary GameSettings

-- * Games and lounge

data Lounge = Lounge
            { _loungeNicksIdle :: Set Nick
            , _loungeGames     :: IntMap (GameSettings, Set Nick, Text) -- ^ (settings, nicks, uuid)
            } deriving (Show, Read)
makeLenses ''Lounge

-- Helpers -------------------------------------------------------------------

atType, atEvent :: Text -> [(Text, A.Value)] -> A.Value
atType  t xs = object ("type"  .= t : xs)
atEvent t xs = object ("event" .= t : xs)

loungeJSON :: Lounge -> [Pair]
loungeJSON (Lounge nicks games) = [ "idle" .= nicks
                                  , "games" .= map gamePairs (itoList games)]

gamePairs :: (Int, (GameSettings, Set Nick, Text)) -> A.Value
gamePairs (i, (GameSettings t, nicks, uuid)) = object
    [ "ident"   .= i
    , "topic"   .= t
    , "uuid"    .= uuid
    , "players" .= nicks ]

-- Instances -----------------------------------------------------------------

instance ToJSON Event where
    toJSON (ClientIdentity nick i tkn) = atType "identity" ["nick"    .= nick, "ident" .= i, "token" .= tkn]
    toJSON (JoinServer nick i tkn mg) = atType "join"      ["nick"    .= nick, "ident" .= i, "token" .= tkn, "game" .= mg]
    toJSON (PartServer nick)       = atType "part"         ["nick"    .= nick]
    toJSON (Message sender cnt)    = atType "msg"          ["from"    .= sender, "content" .= cnt]
    toJSON (Invalid msg)           = atType "invalid"      ["content" .= msg]
    toJSON (LoungeInfo lounge)     = atType "lounge"       (loungeJSON lounge)
    toJSON (GameCreated (i,t,n,u)) = atType "game-created" ["ident"   .= i,    "topic" .= t, "players" .= n, "uuid" .= u]
    toJSON (JoinGame nth nick)     = atType "game-join"    ["nick"    .= nick, "ident" .= nth]
    toJSON (PartGame nick)         = atType "game-part"    ["nick"    .= nick] -- TODO client
    toJSON (InGamePrivateEvent x)  = atType "game-event"   ["events"  .= [x] ]
    toJSON (InGameEvents xs)       = atType "game-event"   ["events"  .= xs  ]
    toJSON (ForceStart nth)        = atType "game-fstart"  ["ident"   .= nth]
    toJSON (InGameAction _)        = atType "game-action"  (error "InGameAction toJSON not implemented")
    toJSON (InternalControl s)     = atType "internal"     ["secret"  .= s]

instance ToJSON GameEvent where
    toJSON ge = case ge of
        DealStarts p pk kyoku                    -> let Object extra = atEvent "round-begin"  ["mypos" .= pk, "player" .= p, "myhand" .= (kyoku^?!sHands.at pk)]
                                                        Object base  = toJSON kyoku
                                                        in Object (extra <> base)
        DealWaitForShout (player, _, secs, xs)   -> atEvent "wait-shout"   ["player"      .= player, "seconds" .= secs, "shouts" .= xs]
        DealWaitForTurnAction (p,_,sec,rs)       -> atEvent "wait-turn"    ["player"      .= p, "seconds" .= sec, "riichi-with" .= rs]
        DealTurnBegins pk                        -> atEvent "turn-changed" ["player-kaze" .= pk]
        DealTurnAction pk turnaction             -> atEvent "turn-action"  ["player-kaze" .= pk, "action" .= turnaction]
        DealTurnShouted pk shout                 -> atEvent "shout"        ["player-kaze" .= pk, "shout" .= shout]
        DealPublicHandChanged pk hand            -> atEvent "hand"         ["player-kaze" .= pk, "hand" .= hand]
        DealPrivateHandChanged _ _ hand          -> atEvent "my-hand"      ["hand"        .= hand]
        DealFlipDora dora                        -> atEvent "flipped-dora" ["tile"        .= dora]
        DealNick pk p nick                       -> atEvent "nick"         ["player"      .= p, "player-kaze" .= pk, "nick" .= nick]
        DealRiichi pk                            -> atEvent "riichi"       ["player-kaze" .= pk]
        DealEnded results                        -> atEvent "end"          ["results"     .= results]
        GamePoints pk n                          -> atEvent "points"       ["player-kaze" .= pk, "points" .= n]
        GameEnded (FinalPoints points)           -> atEvent "game-ended"   ["final_points" .= mapToList points]

instance ToJSON TurnAction where
    toJSON (TurnTileDiscard d) = atType "discard"    ["riichi" .= _dcRiichi d, "tile" .= _dcTile d, "to" .= _dcTo d]
    toJSON (TurnTileDraw w mt) = atType "draw"       ["wanpai" .= w, "tile" .= mt]
    toJSON (TurnAnkan t)       = atType "ankan"      ["tile" .= t]
    toJSON (TurnShouminkan t)  = atType "shouminkan" ["tile" .= t]
    toJSON TurnTsumo           = atType "tsumo"      []

instance ToJSON Player where toJSON (Player k) = toJSON k
instance ToJSON Kaze where toJSON = toJSON . tshow
instance ToJSON Number where toJSON = toJSON . (1 +) . fromEnum
instance ToJSON TileKind where toJSON = toJSON . tshow

instance (ToJSON (m Tile), ToJSON (m FuritenState), ToJSON (m RiichiState), ToJSON (m DrawState), ToJSON (m [Tile]), ToJSON (m Bool))
        => ToJSON (Hand m) where
    toJSON h = object
        [ "called"    .= _handCalled h
        , "discards"  .= _handDiscards h
        , "riichi"    .= _handRiichi h
        , "ippatsu"   .= _handIppatsu h
        , "state"     .= _handState h
        , "picks"     .= _handPicks h

        , "concealed" .= _handConcealed h
        , "furiten"   .= _handFuriten h
        , "can-tsumo" .= _handCanTsumo h
        ]

instance ToJSON Tile where
    toJSON (Suited tk n a) = object [ "type" .= tk, "number" .= n, "aka" .= a ]
    toJSON (Honor h) = object [ "type" .= HonorTile, "ident" .= h]

instance ToJSON Honor where
    toJSON (Sangenpai s) = toJSON (tshow s)
    toJSON (Kazehai k) = toJSON k

instance (ToJSON (m Bool), ToJSON (m [Tile]), ToJSON (m Tile), ToJSON (m RiichiState), ToJSON (m FuritenState), ToJSON (m DrawState)) => ToJSON (Kyoku' m) where
    toJSON x = object
        [ "round"         .= _pRound x
        , "turn"          .= _pTurn x
        , "oja"           .= _pOja x
        , "first-oja"     .= _pFirstOja x
        , "tiles-left"    .= _pWallTilesLeft x
        , "dora"          .= _pDora x
        , "players"       .= map (toJSON *** toJSON) (M.toList $ _pPlayers x)
        , "honba"         .= _pHonba x
        , "in-table"      .= _pRiichi x
        , "results"       .= _pResults x
        , "hands"         .= map (toJSON *** toJSON) (M.toList $ _sHands x)
        , "event-history" .= _sEventHistory x
        ]

instance ToJSON (m Tile) => ToJSON (PickedTile m) where
    toJSON (FromWall t) = atType "from-wall" [ "tile" .= t ]
    toJSON (FromWanpai t) = atType "from-wanpai" [ "tile" .= t ]
    toJSON (AgariTsumo t) = atType "agari-tsumo" [ "tile" .= t ]
    toJSON (AgariCall s) = atType "agari-call" [ "tile" .= shoutTile s, "shout" .= s ]
    toJSON (AgariTsumoWanpai t) = atType "agari-tsumo-wanpai" [ "tile" .= t ]

$(deriveJSON (aesonOptions 0) ''GameSettings)

instance ToJSON a => ToJSON (Map Player a) where
    toJSON = toJSON . itoList

instance ToJSON UUID.UUID where
    toJSON = toJSON . UUID.toText

instance ToJSON a => ToJSON (GameState a)

-- derived

--
$(deriveJSON (aesonOptions 3) ''Discard)
$(deriveJSON (aesonOptions 6) ''Mentsu)
$(deriveJSON (aesonOptions 0) ''MentsuKind)
$(deriveJSON (aesonOptions 0) ''ShoutKind)
$(deriveJSON (aesonOptions 5) ''Shout)
$(deriveJSON (aesonOptions 1) ''KyokuResults)
$(deriveJSON (aesonOptions 2) ''Yaku)
$(deriveJSON (aesonOptions 3) ''Value)
$(deriveJSON (aesonOptions 3) ''ValuedHand)
$(deriveJSON (aesonOptions 0) ''AbortiveDraw)
$(deriveJSON (aesonOptions 0) ''RiichiState)
$(deriveJSON (aesonOptions 0) ''DrawState)
$(deriveJSON (aesonOptions 0) ''FuritenState)

-- FromJSON

instance FromJSON Event where
    parseJSON v@(Object o) = do
        t <- o .: "type"
        case t :: Text of
            "join"         -> JoinServer         <$> o .: "nick" <*> o .: "ident" <*> o .: "token" <*> o .:? "game"
            "part"         -> PartServer         <$> o .: "nick"
            "msg"          -> Message            <$> o .: "from" <*> o .: "content"
            "game-created" -> (\x y z w -> GameCreated (x,y,z,w)) <$> o .: "ident" <*> o .: "topic" <*> o .: "players" <*> o .: "uuid"
            "game-join"    -> JoinGame           <$> o .: "ident" <*> o .: "nick"
            "game-secret"  -> fail "InGamePrivateEvent: This event is only sent from server and not received"
            "game-public"  -> fail "InGameEvents: This event is only sent from server and not received"
            "game-action"  -> InGameAction       <$> parseJSON v
            "game-fstart"  -> ForceStart         <$> o .: "ident"
            "internal"     -> InternalControl    <$> o .: "secret"
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

instance FromJSON TurnAction where
    parseJSON v@(Object o) = do
        t <- o .: "action"
        case t :: Text of
            "discard"    -> TurnTileDiscard <$> parseJSON v
            "draw"       -> TurnTileDraw    <$> o .: "wanpai" <*> o .:? "tile"
            "ankan"      -> TurnAnkan       <$> o .: "tile"
            "shouminkan" -> TurnShouminkan  <$> o .: "tile"
            "tsumo"      -> pure TurnTsumo
            _            -> fail "TurnAction type not recognized"
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
