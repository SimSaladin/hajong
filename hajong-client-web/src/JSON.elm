module JSON where

import GameTypes exposing (..)

import Dict
import Set
import Json.Decode exposing (..)
import Json.Encode as Encode
import Debug

-- Decode ----------------------------------------------------------------------

-- set : Decoder comparable -> Decoder (Set.Set comparable)
set x = map Set.fromList (list x)

-- | map errors to "Invalid" events
decodeEvent : String -> Event
decodeEvent str = case decodeString event str of
   Ok ev   -> ev
   Err err -> Invalid { content = err }

event : Decoder Event
event = ("type" := string) `andThen` eventOfType

eventOfType : String -> Decoder Event
eventOfType eventType = case eventType of
   "identity"     -> object1 (\n -> Identity { nick = n }) nick
   "join"         -> object2 (\n i -> JoinServer { nick = n, ident = i }) nick ident
   "part"         -> object1 (\n -> PartServer { nick = n }) nick
   "msg"          -> object2 (\f c -> Message { from = f, content = c }) from content
   "invalid"      -> object1 (\c -> Invalid { content = c } ) content
   "lounge"       -> object1 (\l -> LoungeInfo { lounge = l }) lounge
   "game-created" -> object1 (\g -> GameCreated { game = g }) gameInfo
   "game-join"    -> object2 (\i n -> JoinGame { ident = i, nick = n }) ident nick
   "game-event"   -> object1 InGameEvents ("events" := list gameEvent)
   _              -> succeed <| Invalid { content = "Received unexpected event of type " ++ eventType }

nick       = "nick"        := string
ident      = "ident"       := int
from       = "from"        := string
content    = "content"     := string
player     = "player"      := int
playerKaze = "player-kaze" := map readKaze string

-- ** Tiles

tile : Decoder Tile
tile = ("type" := string) `andThen` tileOfType

tileMaybe : Decoder (Maybe Tile)
tileMaybe = oneOf [ null Nothing, map Just tile ]

tileOfType : String -> Decoder Tile
tileOfType tileType = case tileType of
    "ManTile"   -> object2 (Suited ManTile) ("number" := int) ("aka" := bool)
    "SouTile"   -> object2 (Suited SouTile) ("number" := int) ("aka" := bool)
    "PinTile"   -> object2 (Suited PinTile) ("number" := int) ("aka" := bool)
    "HonorTile" -> object1 Honor honor
    _           -> Debug.crash <| "Couldn't deserialize tile type `" ++ tileType ++ "'"

honor : Decoder Honor
honor = map honorFrom ("ident" := string)

honorFrom str = case str of
    "Ton"   -> Kazehai Ton
    "Nan"   -> Kazehai Nan
    "Shaa"  -> Kazehai Shaa
    "Pei"   -> Kazehai Pei
    "Haku"  -> Sangenpai Haku
    "Hatsu" -> Sangenpai Hatsu
    "Chun"  -> Sangenpai Chun
    _       -> Debug.crash <| "Couldn't deserialize honor `" ++ str ++ "'"

kaze = string |> map (\x -> case x of
   "Ton"   -> Ton
   "Nan"   -> Nan
   "Shaa"  -> Shaa
   "Pei"   -> Pei
   _       -> Debug.crash <| "Couldn't deserialize kaze `" ++ x ++ "'")

-- ** Lounge

lounge : Decoder LoungeData
lounge = object2 (\i g -> { idle = i, games = g }) ("idle" := (map Set.fromList (list string))) ("games" := list gameInfo)

gameInfo : Decoder GameInfo
gameInfo = object3 (\i t p -> { ident = i, topic = t, players = p}) ("ident" := int) ("topic" := string) ("players" := set nick)

-- ** GameEvent

gameEvent : Decoder GameEvent
gameEvent = "event" := string `andThen` gameEventOfType

gameEventOfType eventType = case eventType of
    "round-begin"  -> object1 RoundPrivateStarts roundState
    "wait-shout"   -> object2 (\s ss   -> RoundPrivateWaitForShout      { seconds = s, shouts = ss                 } ) ("seconds" := int) ("shouts" := list shout)
    "wait-turn"    -> object3 (\p s rw -> RoundPrivateWaitForTurnAction { player = p, seconds = s, riichiWith = rw } ) ("player" := int) ("seconds" := int) ("riichi-with" := list tile)
    "my-hand"      -> object1 (\h      -> RoundPrivateChange            { hand = h                                 } ) ("hand" := hand)
    "turn-changed" -> object1 (\pk     -> RoundTurnBegins               { player_kaze = pk                         } ) playerKaze
    "turn-action"  -> object2 (\pk a   -> RoundTurnAction               { player_kaze = pk, action = a             } ) playerKaze ("action" := turnAction)
    "shout"        -> object2 (\pk s   -> RoundTurnShouted              { player_kaze = pk, shout = s              } ) playerKaze ("shout"  := shout)
    "hand"         -> object2 (\pk h   -> RoundHandChanged              { player_kaze = pk, hand = h               } ) playerKaze ("hand"   := handPublic)
    "nick"         -> object2 (\pk n   -> RoundNick                     { player_kaze = pk, nick = n               } ) playerKaze nick
    "riichi"       -> object1 (\pk     -> RoundRiichi                   { player_kaze = pk                         } ) playerKaze
    "filpped-dora" -> object1 (\t      -> RoundFlippedDora              { tile = t                                 } ) ("tile" := tile)
    "end"          -> object1 RoundEnded                                                                               ("results" := results)
    "points"       -> object2 (\pk po  -> RoundGamePoints               { player_kaze = pk, points = po            } ) playerKaze ("points" := int)
    _              -> Debug.crash <| "Couldn't deserialize game event type `" ++ eventType ++ "'"

-- ** Hand

hand : Decoder Hand
hand = handPublic `andThen` (\hp -> object4 (toHand hp)
   ("concealed" := list tile)
   ("picks"     := list pickedTile)
   ("furiten"   := furitenState)
   ("can-tsumo" := bool)
  )

toHand hp con picks furit canTsumo =
   { called = hp.called, discards = hp.discards, riichiState = hp.riichiState,
     ippatsu = hp.ippatsu, state = hp.state,
   concealed = con,
   picks = picks,
   furiten = furit,
   canTsumo = canTsumo }

handPublic : Decoder HandPublic
handPublic = object5 toHandPublic
   ("state"    := drawState)
   ("called"   := list mentsu)
   ("discards" := list discard)
   ("riichi"   := riichiState)
   ("ippatsu"  := bool)

toHandPublic state called discards riichi ippatsu = { called = called, discards =
   discards, riichiState = riichi, ippatsu = ippatsu, state = state }

playerHand : Decoder (Kaze, HandPublic)
playerHand = tuple2 (,) kaze handPublic

discard : Decoder Discard
discard = object3 Discard
   ("tile" := tile) (oneOf ["to" := map Just kaze, succeed Nothing]) ("riichi" := bool)

riichiState : Decoder RiichiState
riichiState = string |> map (\x -> case x of
   "noriichi"     -> NoRiichi
   "riichi"       -> Riichi
   "doubleriichi" -> DoubleRiichi
   _              -> Debug.crash <| "Couldn't deserialize riichi type `" ++ x ++ "'")

pickedTile : Decoder PickedTile
pickedTile = "type" := string `andThen` \t -> case t of
   "from-wall"     -> object1 FromWall     (maybe ("tile" := tile))
   "from-wanpai"   -> object1 FromWanpai   (maybe ("tile" := tile))
   "agari-tsumo"   -> object1 AgariTsumo   ("tile" := tile)
   "agari-call"    -> object2 AgariCall    ("tile" := tile) ("from-kaze" := kaze)
   "agari-chankan" -> object2 AgariChankan ("tile" := tile) ("from-kaze" := kaze)
   "agari-tsumo-wanpai" -> object1 AgariTsumoWanpai ("tile" := tile)
   _               -> Debug.crash <| "Couldn't deserialize tile kind `" ++ t ++ "'"

furitenState : Decoder FuritenState
furitenState = string |> map (\x -> case x of
   "notfuriten"  -> NotFuriten
   "furiten"     -> Furiten
   "tempfuriten" -> TempFuriten
   _             -> Debug.crash <| "Couldn't deserialize furiten type `" ++ x ++ "'")

drawState : Decoder DrawState
drawState = string |> map (\x -> case x of
   "drawfromwanpai" -> DrawFromWanpai
   "drawfromwall"   -> DrawFromWall
   "drawnone"       -> DrawNone
   _                -> Debug.crash <| "Couldn't deserialize draw state `" ++ x ++ "'")

-- }}}

-- * {{{ Mentsu ------------------------------------------------------
mentsu = object3 Mentsu ("kind" := mentsuKind) ("tiles" := list tile) ("shout" := shoutMaybe)

mentsuKind = map readMentsuKind string

readMentsuKind s = case s of
   "shuntsu" -> Shuntsu
   "koutsu"  -> Koutsu
   "kantsu"  -> Kantsu
   "jantou"  -> Jantou
   _         -> Debug.crash <| "Couldn't deserialize mentsu kind `" ++ s ++ "'"
-- }}}

-- * {{{ Shout -------------------------------------------------------

shout : Decoder Shout
shout = object4 Shout ("kind" := shoutKind) ("from" := kaze) ("tile" := tile) ("to" := list tile)

shoutMaybe = oneOf [null Nothing, map Just shout]

shoutKind = map readShoutKind string

readShoutKind s = case s of
   "pon" -> Pon
   "kan" -> Kan
   "chi" -> Chi
   "ron" -> Ron
   _     -> Debug.crash <| "Couldn't deserialize shout kind `" ++ s ++ "'"
-- }}}

-- * {{{ RoundState -------------------------------------------------------

roundState : Decoder RoundState
roundState = object8 RoundState
    ("mypos"      := kaze)
    ("round"      := tuple2 (,) kaze int)
    ("turn"       := kaze)
    ("player"     := int)
    ("oja"        := int)
    ("first-oja"  := int)
    ("tiles-left" := int)
    ("dora"       := list tile)
    `andThen` (\x -> object7 x
       ("hands"      := list playerHand)
       ("players"    := list players)
       ("myhand"     := hand)
       ("results"    := maybe results)
       ("honba"      := int)
       ("in-table"   := int)
       ("prev-deals" := list (tuple2 (,) kaze int))
    )

points : Decoder (Kaze, Int)
points = tuple2 (,) kaze int

players : Decoder (Kaze, (Player, Int, String))
players = tuple2 (\k (p, ps, n) -> (k, (p, ps, n))) kaze (tuple3 (,,) int int string)
-- }}}

-- * {{{ Results -------------------------------------------------------
results : Decoder RoundResult
results = "type" := string `andThen` resultsOfType

resultsOfType : String -> Decoder RoundResult
resultsOfType t = case t of
   "dealtsumo" -> object2 (\w p -> DealTsumo { winners = w, payers = p }) ("winners" := list winner) ("payers" := list payer)
   "dealron"   -> object2 (\w p -> DealRon   { winners = w, payers = p }) ("winners" := list winner) ("payers" := list payer)
   "dealdraw"  -> object2 (\w p -> DealDraw  { tenpai  = w, nooten = p }) ("tenpais" := list payer)  ("nooten" := list payer)
   _           -> Debug.crash <| "Couldn't deserialize results type `" ++ t ++ "'"

winner : Decoder Winner
winner = tuple3 (\p points h -> Winner p points h) kaze int valuedHand

payer : Decoder Payer
payer = tuple2 (\p v -> Payer p v) kaze int
-- }}}

-- {{{ * Value
handValue : Decoder HandValue
handValue = object5 HandValue
   ("yaku" := list yaku) ("fu" := int) ("han" := int) ("value" := int)
   ("named" := maybe string)

valuedHand : Decoder Valued
valuedHand = object3 Valued ("mentsu" := list mentsu) ("tiles" := list tile) ("value" := handValue)

yaku : Decoder Yaku
yaku = object2 Yaku ("han"  := int) ("name" := string)
-- }}}

-- * {{{ TurnAction -------------------------------------------------------
turnAction : Decoder TurnAction
turnAction {- (Object o) -} = ("type" := string) `andThen` turnActionOfType

turnActionOfType taType = case taType of
    "draw"       -> object2 TurnTileDraw ("wanpai" := bool) ("tile" := maybe tile)
    "discard"    -> object1 TurnTileDiscard discard
    "ankan"      -> object1 TurnAnkan       ("tile" := tile)
    "shouminkan" -> object1 TurnShouminkan  ("tile" := tile)
    "tsumo"      -> succeed TurnTsumo
    _            -> Debug.crash <| "Couldn't deserialize turn action type `" ++ taType ++ "'"
-- }}}

-- {{{ ToJSON ----------------------------------------------------------------------

encodeEvent : Event -> String
encodeEvent = Encode.encode 0 << toJSON_Event

toJSON_Event : Event -> Value
toJSON_Event ev = case ev of
    JoinServer {nick,ident} -> atType "join" [("nick", Encode.string nick), ("ident", Encode.int ident)]
    PartServer {nick}     -> atType "part" [("nick", Encode.string nick)]
    Identity   {nick}     -> atType "identity" [("nick", Encode.string nick)]
    Message{from,content} -> atType "msg" [("from", Encode.string from), ("content", Encode.string content)]
    JoinGame {ident,nick} -> atType "game-join" [("nick", Encode.string nick), ("ident", Encode.int ident)]
    ForceStart {ident}    -> atType "game-fstart" [("ident", Encode.int ident)]
    InGameAction action   -> atType "game-action" <| toJSON_GameAction action
    Noop                  -> atType "noop" []
    _                     -> Debug.crash <| "Couldn't serialize event type, not all are yet implemented!"

toJSON_GameAction : GameAction -> List (String, Value)
toJSON_GameAction a = case a of
   GameTurn (TurnTileDiscard discard)     -> atAction "discard" [("tile", toJSON_Tile discard.tile), ("riichi", Encode.bool discard.riichi)]
   GameTurn (TurnTileDraw dead _)         -> atAction "draw" [("dead", Encode.bool dead)]
   GameTurn (TurnAnkan tile)              -> atAction "ankan" [("tile", toJSON_Tile tile)]
   GameTurn (TurnShouminkan tile)         -> atAction "shouminkan" [("tile", toJSON_Tile tile)]
   GameTurn TurnTsumo                     -> atAction "tsumo" []
   GameShout s                            -> atAction "shout"
               [ ("kind", toJSON_ShoutKind s.shoutKind)
               , ("from", Encode.string <| toString s.shoutFrom)
               , ("tile", toJSON_Tile s.shoutTile)
               , ("to", Encode.list <| List.map toJSON_Tile s.shoutTo) ]
   GameDontCare -> atAction "pass" []

toJSON_ShoutKind sk = Encode.string <| case sk of
   Pon -> "pon"
   Kan -> "kan"
   Chi -> "chi"
   Ron -> "ron"

toJSON_Tile : Tile -> Value
toJSON_Tile tile = case tile of
   Suited suit num aka -> atType (toString suit) [("number", Encode.int num), ("aka", Encode.bool aka)]
   Honor honor         -> atType "HonorTile" <| case honor of
      Kazehai kaze     -> [("ident", Encode.string <| toString kaze)]
      Sangenpai sangen -> [("ident", Encode.string <| toString sangen)]

atType : String -> List (String, Value) -> Value
atType t xs = Encode.object (("type", Encode.string t) :: xs)

atAction : String -> List (String, Value) -> List (String, Value)
atAction t xs = ("action", Encode.string t) :: xs
-- }}}
