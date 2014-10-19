module Connection where

import GameTypes (..)

import WebSocket
import Dict
import Json
import Json (Value, Object, Array, Null, toString, fromString)
import Set (Set)
import Set

connect : Signal Event -> Signal Event
connect output = dropRepeats
    <| fromJson
    <~ WebSocket.connect "ws://localhost:9160" (toString "" . toJson <~ output)
                -- TODO dropRepeats should not be necessary here

-- Event ---------------------------------------------------------------------

-- | Communication with server
data Event = JoinServer { nick : String } -- ^ Nick
           | PartServer { nick : String }
           | Message { from : String, content : String } -- ^ from, content
           | Invalid { content : String }
           | LoungeInfo { lounge : LoungeData }
           | GameCreated { game : GameInfo }
           | JoinGame { ident : Int, nick : String }
           -- | InGamePrivateEvent GameEvent
           | InGameEvents [GameEvent]
            -- One way only?
           | CreateGame String
           | ForceStart { ident : Int }
           | InGameAction GameAction
           | Noop

-- Send

joinServer : String -> Event
joinServer nick = JoinServer {nick = nick}

createGame : String -> Event
createGame topic = CreateGame topic

joinGame : Int -> Event
joinGame n = JoinGame { nick = "", ident = n }

forceStart : Int -> Event
forceStart n = ForceStart { ident = n }

-- Receive -------------------------------------------------------------------

type KindaState a = {a | gameWait : Maybe Int, mynick : String, lounge : LoungeData }

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

-- From JSON

fromJson : String -> Event
fromJson str = maybe (Invalid { content = str }) fromJson' <| fromString str

fromJust : Maybe a -> a
fromJust (Just x) = x

fromJson' : Value -> Event
fromJson' (Object o) = case "type" .: o |> justString of

    "join"         -> JoinServer  <| hasNick o {}
    "part"         -> PartServer  <| hasNick o {}
    "msg"          -> Message     <| hasContent o <| hasFrom o {}
    "invalid"      -> Invalid     <| hasContent o {}
    "lounge"       -> LoungeInfo  <| hasLounge o {}
    "game-created" -> GameCreated <| hasGame o {}
    "game-join"    -> JoinGame    <| hasNick o { ident = justInt <| "ident" .: o }
    "game-event"   -> InGameEvents <| (\(Array xs) -> map parseGameEvent xs)
                                   <| Dict.getOrFail "events" o

    t -> Invalid { content = "Received unexpected " ++ t }

-- * To JSON

toJson : Event -> Value
toJson ev = case ev of
    JoinServer {nick}     -> atType "join" [("nick", Json.String nick)]
    PartServer {nick}     -> atType "part" [("nick", Json.String nick)]
    Message{from,content} -> atType "msg" [("from", Json.String from), ("content", Json.String content)]
    CreateGame g          -> atType "game-create" [("topic", Json.String g)]
    JoinGame {ident,nick} -> atType "game-join" [("nick", Json.String nick), ("ident", Json.Number <| toFloat ident)]
    ForceStart {ident}    -> atType "game-fstart" [("ident", Json.Number <| toFloat ident)]

atType : String -> [(String, Value)] -> Value
atType t xs = Object (Dict.fromList <| ("type", Json.String t) :: xs)

-- Parsers

n .: o = Dict.getOrFail n o

justString (Json.String s) = s
justInt (Json.Number n) = floor n
justBool (Json.Boolean b) = b

justTile (Object o) = case "type" .: o |> justString of
    "ManTile" -> Suited Man ("number" .: o |> justInt) (justBool <| "aka" .: o)
    "SouTile" -> Suited Sou ("number" .: o |> justInt) (justBool <| "aka" .: o)
    "PinTile" -> Suited Pin ("number" .: o |> justInt) (justBool <| "aka" .: o)
    "HonorTile" -> Honor <| hasHonor o

hasHonor o = case "ident" .: o |> justString of
    "Ton"   -> Kazehai Ton
    "Nan"   -> Kazehai Nan
    "Shaa"  -> Kazehai Shaa
    "Pei"   -> Kazehai Pei
    "Haku"  -> Sangenpai Haku
    "Hatsu" -> Sangenpai Hatsu
    "Chun"  -> Sangenpai Chun


maybeTile _ = Nothing

hasNick o s    = { s | nick    = "nick"    .: o |> justString }
hasFrom o s    = { s | from    = "from"    .: o |> justString }
hasContent o s = { s | content = "content" .: o |> justString }
hasPlayer o s  = { s | player  = "player"  .: o |> justString |> readKaze }
hasLounge o s  = { s | lounge  = parseLoungeData o }
hasGame o s    = { s | game    = parseGame o }

parseLoungeData o =
    { idle  = nickSet <| "idle" .: o
    , games = gameList <| "games" .: o
    }

nickSet (Array xs) = map (\(Json.String s) -> s) xs |> Set.fromList

gameList : Value -> [GameInfo]
gameList (Array xs) = map (\(Object o) -> parseGame o) xs

parseGame o = { ident   = justInt    <| "ident" .: o
              , topic   = justString <| "topic" .: o
              , players = nickSet    <| "players" .: o
              }

parseGameEvent o = case "event" .: o |> justString of
    "round-begin"  -> RoundPrivateStarts <| parseRoundState o
    "waiting"      -> RoundPrivateWaitForShout (justInt <| "seconds" .: o)
    "my-hand"      -> RoundPrivateChange <| hasPlayer o <| { hand = "hand" .: o |> parseHand }
    "turn-changed" -> RoundTurnBegins <| hasPlayer o
    "turn-action"  -> RoundTurnAction <| hasPlayer o { action = "action" .: o |> parseTurnAction }
    "shout"        -> RoundTurnShouted <| hasPlayer o { shout = "shout" .: o |> parseShout }
    "hand"         -> RoundHandChanged <| hasPlayer o { hand = "hand" .: o |> parseHand }
    "end"          -> RoundEnded RoundResult

parseHand o = {}

parseShout o = {}

parseRoundState o = case "gamestate" .: o of
    Object g -> 
        { mypos = "player" .: o |> readKaze . justString
        , round = "round" .: g |> readKaze . justString
        , dealer = "oja" .: g |> readKaze . justString
        , turn = "turn" .: g |> readKaze . justString
        , dora = "dora" .: g |> (\(Array xs) -> map justTile xs)
        , tilesleft = "tiles-left" .: g |> justInt
        , hands = "hands" .: o |> (\(Array xs) -> Dict.fromList <| map justHand xs)
        , points = "players" .: g |> parsePlayers
        , results = "results" .: g |> parseResults
        }

justHand (Array [a, b]) = 

parsePlayers (Array xs) = map parsePlayer xs

parsePlayer (Array [a,b,c]) = (a, b, c)

parseResults (Object o) = { endKind = Tsumo, winners = [], payers = [] } -- TODO

parseTurnAction o = case "type" .: o |> justString of
    "draw"    -> TurnTileDraw ("wanpai" .: o |> justBool) (Dict.get "tile" o |> maybeTile)
    "discard" -> TurnTileDiscard ("riichi" .: o |> justBool) ("tile" .: o |> justTile)
    "ankan"   -> TurnAnkan <| ("tile" .: o |> justTile)
