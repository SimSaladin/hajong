module JSON where

import GameTypes (..)

import Dict
import Set
import Maybe (maybe)
import Json (Value(..), toString, fromString)
import Debug

-- FromJSON --------------------------------------------------------------------

fromJSON_Event : String -> Event
fromJSON_Event str = maybe (Invalid { content = str }) parseEvent <| fromString str

fromJust : Maybe a -> a
fromJust (Just x) = x

-- ToJSON ----------------------------------------------------------------------

toJSON_Event : Event -> Value
toJSON_Event ev = case ev of
    JoinServer {nick}     -> atType "join" [("nick", String nick)]
    PartServer {nick}     -> atType "part" [("nick", String nick)]
    Identity   {nick}     -> atType "identity" [("nick", String nick)]
    Message{from,content} -> atType "msg" [("from", String from), ("content", String content)]
    JoinGame {ident,nick} -> atType "game-join" [("nick", String nick), ("ident", Number <| toFloat ident)]

    CreateGame g          -> atType "game-create" [("topic", String g)]
    ForceStart {ident}    -> atType "game-fstart" [("ident", Number <| toFloat ident)]
    InGameAction action   -> atType "game-action" <| toJSON_GameAction action
    Noop                  -> atType "noop" []

toJSON_GameAction : GameAction -> [(String, Value)]
toJSON_GameAction a = case a of
   GameTurn (TurnTileDiscard riichi tile) -> atAction "discard" [("tile", toJSON_Tile tile), ("riichi", Boolean riichi)]
   GameTurn (TurnTileDraw dead _)         -> atAction "draw" [("dead", Boolean dead)]
   GameTurn (TurnAnkan tile)              -> atAction "ankan" [("tile", toJSON_Tile tile)]
   GameShout s                            -> atAction "shout"
               [ ("shout", toJSON_ShoutKind s.shoutKind)
               , ("from", String <| show s.shoutFrom)
               , ("tile", toJSON_Tile s.shoutTile)
               , ("into", Array <| map toJSON_Tile s.shoutTo) ]
   GameDontCare -> atAction "pass" []

toJSON_ShoutKind sk = String <| case sk of
   Pon -> "pon"
   Kan -> "kan"
   Chi -> "chi"
   Ron -> "ron"

toJSON_Tile : Tile -> Value
toJSON_Tile tile = case tile of
   Suited suit num aka -> atType (show suit) [("number", Number <| toFloat num), ("aka", Boolean aka)]
   Honor honor -> atType "HonorTile" <| case honor of
      Kazehai kaze -> [("ident", String <| show kaze)]
      Sangenpai sangen -> [("ident", String <| show sangen)]

atType : String -> [(String, Value)] -> Value
atType t xs = Object (Dict.fromList <| ("type", String t) :: xs)

atAction : String -> [(String, Value)] -> [(String, Value)]
atAction t xs = ("action", String t) :: xs

-- Parsers ---------------------------------------------------------------------

-- ** Helpers ----------------------------------------------------
n .: o = Dict.getOrFail n o

parseString x = case x of
   String s -> s
   _        -> Debug.crash <| "JSON.parseString: not a string: " ++ show x

parseInt x = case x of
   Number n -> floor n
   _        -> Debug.crash <| "JSON.parseInt: not int: " ++ show x

parseBool x = case x of
   Boolean b -> b
   _         -> Debug.crash <| "JSON.parseBool: not bool: " ++ show x

parseBoolMaybe x = case x of
   Boolean b -> Just b
   Null      -> Nothing

withArray f (Array xs) = map f xs

-- ** Event ------------------------------------------------------
parseEvent : Value -> Event
parseEvent (Object o) = case "type" .: o |> parseString of
    "identity"     -> Identity     <| hasNick o {}
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
    "ManTile"   -> Suited ManTile ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "SouTile"   -> Suited SouTile ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "PinTile"   -> Suited PinTile ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "HonorTile" -> Honor <| hasHonor o

parseTileMaybe : Value -> Maybe Tile
parseTileMaybe x = case x of
   Object _  -> Just <| parseTile x
   Null      -> Nothing

parseDiscardTile : Value -> (Tile, Maybe Kaze)
parseDiscardTile (Array [a, b]) =
   let player = case b of
         Null -> Nothing
         String s -> readKaze s |> Just
      in (parseTile a, player)

parseKaze : Value -> Kaze
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
hasNick o s        = { s | nick    = "nick"        .: o |> parseString }
hasFrom o s        = { s | from    = "from"        .: o |> parseString }
hasContent o s     = { s | content = "content"     .: o |> parseString }
hasPlayer o s      = { s | player  = "player"      .: o |> parseInt }
hasPlayerKaze o s  = { s | player_kaze  = "player-kaze" .: o |> parseKaze }
hasLounge o s      = { s | lounge  = parseLoungeData o }
hasGame o s        = { s | game    = parseGame o }

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
              , players = parseNicks  <| "players" .: o
              }

-- ** GameEvents ------------------------------------------------
parseGameEvent : Value -> GameEvent
parseGameEvent (Object o) = case "event" .: o |> parseString of
    "round-begin"  -> RoundPrivateStarts            <| parseRoundState o
    "wait-shout"   -> RoundPrivateWaitForShout      <| { seconds = "seconds" .: o |> parseInt
                                                       , shouts = "shouts" .: o |> withArray parseShout
                                                       }
    "wait-turn"    -> RoundPrivateWaitForTurnAction <| hasPlayer o { seconds = "seconds" .: o |> parseInt }
    "my-hand"      -> RoundPrivateChange            <| hasPlayer o { hand    = "hand"    .: o |> parseHand }
    "turn-changed" -> RoundTurnBegins               <| hasPlayerKaze o { }
    "turn-action"  -> RoundTurnAction               <| hasPlayerKaze o { action  = "action"  .: o |> parseTurnAction }
    "shout"        -> RoundTurnShouted              <| hasPlayerKaze o { shout   = "shout"   .: o |> parseShout }
    "hand"         -> RoundHandChanged              <| hasPlayerKaze o { hand    = "hand" .: o |> parsePublicHand }
    "end"          -> RoundEnded                    <| fromJust <| parseResults <| "results" .: o
    "nick"         -> RoundNick                     <| hasPlayerKaze o { nick = "nick" .: o |> parseString }

-- * Hand -------------------------------------------------------
parseHand : Value -> Hand
parseHand v =
   let x = parsePublicHand v
   in
      case v of
         Object o ->
            { concealed   = "concealed" .: o |> withArray parseTile
            , pick        = "pick"      .: o |> parseTileMaybe
            , furiten     = "furiten"   .: o |> parseBoolMaybe
            , called      = x.called
            , discards    = x.discards
            , riichi      = x.riichi
            }

parsePublicHand : Value -> HandPublic
parsePublicHand (Object o) = 
   { called      = "called"       .: o |> withArray parseMentsu
   , discards    = "discards"     .: o |> withArray parseDiscardTile
   , riichi      = "riichi"       .: o |> parseBool
   }

parsePlayerHand : Value -> (Kaze, HandPublic)
parsePlayerHand (Array [a, b]) = (parseKaze a, parsePublicHand b)

-- * Mentsu ------------------------------------------------------
parseMentsu : Value -> Mentsu
parseMentsu (Object o) = Mentsu
   (parseMentsuKind <| "type" .: o)
   (parseTile       <| "tile" .: o)
   (parseShoutMaybe <| "shouted" .: o)

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
        { mypos     = "kaze"       .: o    |> parseKaze
        , round     = "round"      .: game |> parseKaze
        , turn      = "turn"       .: game |> parseKaze
        , player    = "player"     .: o    |> parseInt
        , oja       = "oja"        .: game |> parseInt
        , firstoja  = "first-oja"  .: game |> parseInt
        , tilesleft = "tiles-left" .: game |> parseInt
        , dora      = "dora"       .: game |> withArray parseTile
        , hands     = "hands"      .: o    |> withArray parsePlayerHand
        , players   = "players"    .: game |> withArray parsePlayers
        , myhand    = "myhand"     .: o    |> parseHand
        , results   = "results"    .: game |> parseResults
        , actions   = [] -- TODO receive actions?
        }

parsePoints : Value -> (Kaze, Int)
parsePoints (Array [a, b]) = (parseKaze a, parseInt b)

parsePlayers : Value -> (Kaze, (Player, Int, String))
parsePlayers (Array [p, Array [k, ps, n]]) = (parseKaze k, (parseInt p, parseInt ps, parseString n))

-- * RoundResult -------------------------------------------------------
parseResults : Value -> Maybe RoundResult
parseResults v = case v of
   Object o -> Just { endKind = "type"    .: o |> parseEndKind
                    , winners = "winners" .: o |> withArray parseKaze
                    , payers  = "payers"  .: o |> withArray parseKaze
                    }
   Null     -> Nothing

parseEndKind : Value -> EndKind
parseEndKind (String s) = case s of
   "tsumo" -> Tsumo
   "ron"   -> ByRon
   "draw"  -> Draw

-- * TurnAction -------------------------------------------------------
parseTurnAction : Value -> TurnAction
parseTurnAction (Object o) = case "type" .: o |> parseString of
    "draw"    -> TurnTileDraw    ("wanpai" .: o |> parseBool) ("tile" .: o |> parseTileMaybe)
    "discard" -> TurnTileDiscard ("riichi" .: o |> parseBool) ("tile" .: o |> parseTile)
    "ankan"   -> TurnAnkan       ("tile"   .: o |> parseTile)
