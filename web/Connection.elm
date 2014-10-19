module Connection where

import GameTypes (..)

import Dict
import Maybe (maybe)
import Json (Value(..), toString, fromString)
import Json
import Set (Set)
import Set
import WebSocket

connect : Signal Event -> Signal Event
connect output = dropRepeats
    <| fromJSON_Event
    <~ WebSocket.connect "ws://localhost:9160" (toString "" << toJSON_Event <~ output)
                -- TODO dropRepeats should not be necessary here

-- Event ---------------------------------------------------------------------

-- | Communication with server
data Event = JoinServer  { nick : String } -- ^ Nick
           | PartServer  { nick : String }
           | Message     { from : String, content : String } -- ^ from, content
           | Invalid     { content : String }
           | LoungeInfo  { lounge : LoungeData }
           | GameCreated { game : GameInfo }
           | JoinGame    { ident : Int, nick : String }
           | InGameEvents [GameEvent]

            -- To server only
           | CreateGame String
           | ForceStart  { ident : Int }
           | InGameAction GameAction
           | Noop -- TODO work around elm WS lib signal limitations

-- Create -------------------------------------------------------------------

joinServer : String -> Event
joinServer nick = JoinServer {nick = nick}

createGame : String -> Event
createGame topic = CreateGame topic

joinGame : Int -> Event
joinGame n = JoinGame { nick = "", ident = n }

forceStart : Int -> Event
forceStart n = ForceStart { ident = n }

-- Receive -------------------------------------------------------------------

type KindaState a = { a | gameWait : Maybe Int
                        , mynick   : String
                        , lounge   : LoungeData
                    }

processEvent : Event -> KindaState a -> KindaState a
processEvent event gameState = case event of
        JoinServer {nick}   -> { gameState | lounge <- addIdle nick gameState.lounge }
        PartServer {nick}   -> { gameState | lounge <- deleteNick nick gameState.lounge }
        LoungeInfo {lounge} -> { gameState | lounge <- lounge }
        GameCreated {game}  -> { gameState | lounge <- addGame game gameState.lounge }
        JoinGame {nick, ident} ->
            { gameState | lounge   <- addJoinedGame ident nick gameState.lounge
                        , gameWait <-
                            if gameState.mynick == nick
                                then Just ident
                                else gameState.gameWait
            }
        _ -> gameState

-- Helpers

addGame g l    = { l | games <- l.games ++ [g] }
addIdle n l    = { l | idle  <- Set.insert n l.idle }

addJoinedGame i n l =
    { l | games <- map (\g -> if g.ident == i then { g | players <- Set.insert n g.players } else g) l.games
        , idle  <- Set.remove n l.idle
    }

deleteNick n l = { l | idle  <- Set.remove n l.idle
                     , games <- map (\g -> { g | players <- Set.remove n g.players }) l.games
                 }

-- FromJSON --------------------------------------------------------------------

fromJSON_Event : String -> Event
fromJSON_Event str = maybe (Invalid { content = str }) parseEvent <| fromString str

fromJust : Maybe a -> a
fromJust (Just x) = x

-- ToJSON ----------------------------------------------------------------------

toJSON_Event : Event -> Value
toJSON_Event ev = case ev of
    JoinServer {nick}     -> atType "join" [("nick", Json.String nick)]
    PartServer {nick}     -> atType "part" [("nick", Json.String nick)]
    Message{from,content} -> atType "msg" [("from", Json.String from), ("content", Json.String content)]
    CreateGame g          -> atType "game-create" [("topic", Json.String g)]
    JoinGame {ident,nick} -> atType "game-join" [("nick", Json.String nick), ("ident", Json.Number <| toFloat ident)]
    ForceStart {ident}    -> atType "game-fstart" [("ident", Json.Number <| toFloat ident)]

atType : String -> [(String, Value)] -> Value
atType t xs = Object (Dict.fromList <| ("type", Json.String t) :: xs)

-- Parsers ---------------------------------------------------------------------

-- ** Helpers ----------------------------------------------------
n .: o = Dict.getOrFail n o

parseString (Json.String s)  = s
parseInt    (Json.Number n)  = floor n
parseBool   (Json.Boolean b) = b

parseBoolMaybe x = case x of
   Json.Boolean b -> Just b
   Json.Null      -> Nothing

withArray f (Json.Array xs) = map f xs

-- ** Event ------------------------------------------------------
parseEvent : Value -> Event
parseEvent (Object o) = case "type" .: o |> parseString of
    "join"         -> JoinServer   <| hasNick o {}
    "part"         -> PartServer   <| hasNick o {}
    "msg"          -> Message      <| hasContent o <| hasFrom o {}
    "invalid"      -> Invalid      <| hasContent o {}
    "lounge"       -> LoungeInfo   <| hasLounge o {}
    "game-created" -> GameCreated  <| hasGame o {}
    "game-join"    -> JoinGame     <| hasNick o { ident = parseInt <| "ident" .: o }
    "game-event"   -> InGameEvents <| withArray parseGameEvent <| "events" .: o
    t              -> Invalid { content = "Received unexpected " ++ t }

-- ** Tiles -----------------------------------------------------
parseTile : Value -> Tile
parseTile (Object o) = case "type" .: o |> parseString of
    "ManTile"   -> Suited Man ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "SouTile"   -> Suited Sou ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "PinTile"   -> Suited Pin ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "HonorTile" -> Honor <| hasHonor o

parseTileMaybe : Value -> Maybe Tile
parseTileMaybe x = case x of
   Object _  -> Just <| parseTile x
   Json.Null -> Nothing

parseKaze = readKaze << parseString

hasHonor o = case "ident" .: o |> parseString of
    "Ton"   -> Kazehai Ton
    "Nan"   -> Kazehai Nan
    "Shaa"  -> Kazehai Shaa
    "Pei"   -> Kazehai Pei
    "Haku"  -> Sangenpai Haku
    "Hatsu" -> Sangenpai Hatsu
    "Chun"  -> Sangenpai Chun

-- ** General fields ------------------------------------------
hasNick o s    = { s | nick    = "nick"    .: o |> parseString }
hasFrom o s    = { s | from    = "from"    .: o |> parseString }
hasContent o s = { s | content = "content" .: o |> parseString }
hasPlayer o s  = { s | player  = "player"  .: o |> parseKaze }
hasLounge o s  = { s | lounge  = parseLoungeData o }
hasGame o s    = { s | game    = parseGame o }

-- ** Lounge --------------------------------------------------
parseLoungeData o =
    { idle  = parseNicks     <| "idle"  .: o
    , games = parseGameInfos <| "games" .: o
    }

parseNicks : Value -> Set.Set String
parseNicks = Set.fromList << withArray parseString

parseGameInfos : Value -> [GameInfo]
parseGameInfos = withArray (\(Object o) -> parseGame o)

parseGame o = { ident   = parseInt    <| "ident" .: o
              , topic   = parseString <| "topic" .: o
              , players = parseNicks <| "players" .: o
              }

-- ** GameEvents ------------------------------------------------
parseGameEvent : Value -> GameEvent
parseGameEvent (Object o) = case "event" .: o |> parseString of
    "round-begin"  -> RoundPrivateStarts       <| parseRoundState o
    "waiting"      -> RoundPrivateWaitForShout                { seconds = "seconds" .: o |> parseInt }
    "my-hand"      -> RoundPrivateChange       <| hasPlayer o { hand    = "hand"    .: o |> parseHand }
    "turn-changed" -> RoundTurnBegins          <| hasPlayer o { }
    "turn-action"  -> RoundTurnAction          <| hasPlayer o { action  = "action"  .: o |> parseTurnAction }
    "shout"        -> RoundTurnShouted         <| hasPlayer o { shout   = "shout"   .: o |> parseShout }
    "hand"         -> RoundHandChanged         <| hasPlayer o { hand    = "hand"    .: o |> parseHand }
    "end"          -> RoundEnded               <| fromJust <| parseResults <| "results" .: o

-- * Hand -------------------------------------------------------
parseHand : Value -> Hand
parseHand v =
   let x = parsePublicHand v
   in
      case v of
         Object o ->
            { concealed   = "cocealed" .: o |> withArray parseTile
            , pick        = "pick" .: o |> parseTileMaybe
            , furiten     = "furiten"  .: o |> parseBoolMaybe
            , called      = x.called
            , discards    = x.discards
            , riichi      = x.riichi
            , turnDiscard = x.turnDiscard
            }

parsePublicHand : Value -> HandPublic
parsePublicHand (Object o) = 
   { called      = "called"      .: o |> withArray parseMentsu
   , discards    = "discards"    .: o |> withArray parseTile
   , riichi      = "riichi"      .: o |> parseBool
   , turnDiscard = "turnDiscard" .: o |> parseTileMaybe
   }

parsePlayerHand : Value -> (Kaze, HandPublic)
parsePlayerHand (Array [a, b]) = (parseKaze a, parsePublicHand b)

-- * Mentsu ------------------------------------------------------
parseMentsu : Value -> Mentsu
parseMentsu (Object o) = Mentsu
   (parseMentsuKind <| "type" .: o)
   (parseTile       <| "tile" .: o)
   (parseShoutMaybe <| "from" .: o)

parseMentsuKind : Value -> MentsuKind
parseMentsuKind (String s) = case s of
   "Shuntsu" -> Shuntsu
   "Koutsu"  -> Koutsu
   "Kantsu"  -> Kantsu
   "Jantou"  -> Jantou

-- * Shout -------------------------------------------------------
parseShout : Value -> Shout
parseShout (Object o) =
   { shoutKind = "type" .: o |> parseShoutKind
   , shoutFrom = "from" .: o |> parseKaze
   , shoutTile = "tile" .: o |> parseTile
   , shoutTo   = "to"   .: o |> withArray parseTile
   }

parseShoutMaybe : Value -> Maybe Shout
parseShoutMaybe v = case v of
   Null     -> Nothing
   Object _ -> Just <| parseShout v

parseShoutKind : Value -> ShoutKind
parseShoutKind (String s) = case s of
   "pon" -> Pon
   "kan" -> Kan
   "chi" -> Chi
   "ron" -> Ron

-- * RoundState -------------------------------------------------------
parseRoundState : Dict.Dict String Value -> RoundState
parseRoundState o = case "gamestate" .: o of
    Object game -> 
        { mypos     = "player"     .: o    |> parseKaze
        , hands     = "hands"      .: o    |> withArray parsePlayerHand
        , myhand    = "myhand"     .: o    |> parseHand
        , players   = "players"    .: o    |> withArray parsePlayer
        , round     = "round"      .: game |> parseKaze
        , dealer    = "oja"        .: game |> parseKaze
        , turn      = "turn"       .: game |> parseKaze
        , dora      = "dora"       .: game |> withArray parseTile
        , tilesleft = "tiles-left" .: game |> parseInt
        , results   = "results"    .: game |> parseResults
        }

parsePlayer : Value -> (Kaze, String, Int)
parsePlayer (Array [a, b, c]) = (readKaze <| parseString a, parseString b, parseInt c)

-- * RoundResult -------------------------------------------------------
parseResults : Value -> Maybe RoundResult
parseResults v = case v of
   Object o -> Just { endKind = "type"    .: o |> parseEndKind
                    , winners = "winners" .: o |> withArray parseKaze
                    , payers  = "payers"  .: o |> withArray parseKaze
                    }
   Null     -> Nothing

parseEndKind : Value -> EndKind
parseEndKind (Json.String s) = case s of
   "tsumo" -> Tsumo
   "ron"   -> ByRon
   "draw"  -> Draw

-- * TurnAction -------------------------------------------------------
parseTurnAction : Value -> TurnAction
parseTurnAction (Object o) = case "type" .: o |> parseString of
    "draw"    -> TurnTileDraw    ("wanpai" .: o |> parseBool) ("tile" .: o |> parseTileMaybe)
    "discard" -> TurnTileDiscard ("riichi" .: o |> parseBool) ("tile" .: o |> parseTile)
    "ankan"   -> TurnAnkan       ("tile"   .: o |> parseTile)

