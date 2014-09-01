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
           | JoinGame Int String -- ^ Game lounge
           | InGamePrivateEvent GameEvent
           | InGameEvents [GameEvent]
            -- One way only?
           | CreateGame String
           | InGameAction GameAction

-- Send

joinServer : String -> Event
joinServer nick = JoinServer {nick = nick}

createGame : String -> Event
createGame topic = CreateGame topic

-- Receive -------------------------------------------------------------------

processEvent : Event -> {a | lounge : LoungeData } -> {a | lounge : LoungeData }
processEvent event gameState = case event of
        JoinServer {nick}   -> { gameState | lounge <- addIdle nick gameState.lounge }
        PartServer {nick}   -> { gameState | lounge <- deleteNick nick gameState.lounge }
        LoungeInfo {lounge} -> { gameState | lounge <- lounge }
        GameCreated {game}  -> { gameState | lounge <- addGame game gameState.lounge }
        _ -> gameState

-- Helpers

addIdle n l    = { l | idle  <- Set.insert n l.idle }
deleteNick n l = { l | idle  <- Set.remove n l.idle }
addGame g l    = { l | games <- g :: l.games }

-- JSON conversion

fromJson : String -> Event
fromJson str = maybe (Invalid { content = str }) fromJson' <| fromString str

fromJust : Maybe a -> a
fromJust (Just x) = x

fromJson' : Value -> Event
fromJson' (Object o) = case "type" .: o |> justString of

    "join"    -> JoinServer <| hasNick o {}
    "part"    -> PartServer <| hasNick o {}
    "msg"     -> Message <| hasContent o <| hasFrom o {}
    "invalid" -> Invalid <| hasContent o {}

    "lounge"  -> LoungeInfo <| hasLounge o {}

    "game-created" -> GameCreated <| hasGame o {}
    "game-join" -> JoinGame (justInt <| Dict.getOrFail "ident" o)
                            (justString <| Dict.getOrFail "nick" o)
    "game-secret" -> InGamePrivateEvent <| parseGameEvent o
    "game-event"  -> InGameEvents <| (\(Array xs) -> map parseGameEvent xs)
                                  <| Dict.getOrFail "events" o

    t -> Invalid { content = "Received unexpected " ++ t }

toJson : Event -> Value
toJson ev = case ev of
    JoinServer {nick} -> Object (Dict.fromList [("type", Json.String "join"), ("nick", Json.String nick)])
    CreateGame g -> Object (Dict.fromList [("type", Json.String "game-create"), ("topic", Json.String g)])

-- Parsers

n .: o = Dict.getOrFail n o

hasNick o s = { s | nick = "nick" .: o |> justString }
hasFrom o s = { s | from = "from" .: o |> justString }
hasContent o s = { s | content = "content" .: o |> justString }

hasLounge o s = { s | lounge = parseLoungeData o }
hasGame o s = { s | game = parseGame o }

parseLoungeData o =
    { idle  = nickSet <| "idle" .: o
    , games = gameList <| "games" .: o
    }

nickSet (Array xs) = map (\(Json.String s) -> s) xs |> Set.fromList

gameList : Value -> [GameInfo]
gameList (Array xs) = map (\(Object o) -> parseGame o) xs

justString (Json.String s) = s
justInt (Json.Number n) = floor n

parseGame o = { ident = justInt <| "ident" .: o
              , topic = justString <| "topic" .: o
              , players = nickSet <| "players" .: o
              }

