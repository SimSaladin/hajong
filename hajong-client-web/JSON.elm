module JSON where

import GameTypes (..)

import Dict
import Set
import Maybe (maybe)
import Json (Value(..), toString, fromString)
import Debug
import Util (..)

-- FromJSON --------------------------------------------------------------------

fromJSON_Event : String -> Event
fromJSON_Event str = maybe (Invalid { content = str }) parseEvent <| fromString str

-- {{{ ToJSON ----------------------------------------------------------------------

toJSON_Event : Event -> Value
toJSON_Event ev = case ev of
    JoinServer {nick,ident} -> atType "join" [("nick", String nick), ("ident", Number <| toFloat ident)]
    PartServer {nick}     -> atType "part" [("nick", String nick)]
    Identity   {nick}     -> atType "identity" [("nick", String nick)]
    Message{from,content} -> atType "msg" [("from", String from), ("content", String content)]
    JoinGame {ident,nick} -> atType "game-join" [("nick", String nick), ("ident", Number <| toFloat ident)]

    ForceStart {ident}    -> atType "game-fstart" [("ident", Number <| toFloat ident)]
    InGameAction action   -> atType "game-action" <| toJSON_GameAction action
    Noop                  -> atType "noop" []

toJSON_GameAction : GameAction -> [(String, Value)]
toJSON_GameAction a = case a of
   GameTurn (TurnTileDiscard discard)     -> atAction "discard" [("tile", toJSON_Tile discard.tile), ("riichi", Boolean discard.riichi)]
   GameTurn (TurnTileDraw dead _)         -> atAction "draw" [("dead", Boolean dead)]
   GameTurn (TurnAnkan tile)              -> atAction "ankan" [("tile", toJSON_Tile tile)]
   GameTurn (TurnShouminkan tile)         -> atAction "shouminkan" [("tile", toJSON_Tile tile)]
   GameTurn TurnTsumo                     -> atAction "tsumo" []
   GameShout s                            -> atAction "shout"
               [ ("kind", toJSON_ShoutKind s.shoutKind)
               , ("from", String <| show s.shoutFrom)
               , ("tile", toJSON_Tile s.shoutTile)
               , ("to", Array <| map toJSON_Tile s.shoutTo) ]
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
-- }}}

-- Parsers ---------------------------------------------------------------------

-- ** {{{ Helpers ----------------------------------------------------
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

parseStringMaybe x = case x of
   String s -> Just s
   Null -> Nothing

withArray f (Array xs) = map f xs

parsePlayer = parseInt
-- }}}

-- ** {{{ Event ------------------------------------------------------
parseEvent : Value -> Event
parseEvent (Object o) = case "type" .: o |> parseString of
    "identity"     -> Identity     <| hasNick o {}
    "join"         -> JoinServer   <| hasNick o { ident = parseInt <| "ident" .: o }
    "part"         -> PartServer   <| hasNick o {}
    "msg"          -> Message      <| hasContent o <| hasFrom o {}
    "invalid"      -> Invalid      <| hasContent o {}
    "lounge"       -> LoungeInfo   <| hasLounge o {}
    "game-created" -> GameCreated  <| hasGame o {}
    "game-join"    -> JoinGame     <| hasNick o { ident = parseInt <| "ident" .: o }
    "game-event"   -> InGameEvents <| withArray parseGameEvent <| "events" .: o
    t              -> Invalid { content = "Received unexpected " ++ t }
-- }}}

-- ** {{{ Tiles -----------------------------------------------------
parseTile : Value -> Tile
parseTile (Object o) = case "type" .: o |> parseString of
    "ManTile"   -> Suited ManTile ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "SouTile"   -> Suited SouTile ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "PinTile"   -> Suited PinTile ("number" .: o |> parseInt) (parseBool <| "aka" .: o)
    "HonorTile" -> Honor <| hasHonor o

parseTileMaybe : Value -> Maybe Tile
parseTileMaybe x = case x of
    Object _    -> Just <| parseTile x
    Null        -> Nothing

hasHonor o = case "ident" .: o |> parseString of
    "Ton"       -> Kazehai Ton
    "Nan"       -> Kazehai Nan
    "Shaa"      -> Kazehai Shaa
    "Pei"       -> Kazehai Pei
    "Haku"      -> Sangenpai Haku
    "Hatsu"     -> Sangenpai Hatsu
    "Chun"      -> Sangenpai Chun

parseKaze : Value -> Kaze
parseKaze = readKaze << parseString
-- }}}

-- ** {{{ General fields ------------------------------------------
hasNick o s        = { s | nick         = "nick"        .: o |> parseString }
hasFrom o s        = { s | from         = "from"        .: o |> parseString }
hasContent o s     = { s | content      = "content"     .: o |> parseString }
hasPlayer o s      = { s | player       = "player"      .: o |> parseInt }
hasPlayerKaze o s  = { s | player_kaze  = "player-kaze" .: o |> parseKaze }
hasLounge o s      = { s | lounge       = parseLoungeData o }
hasGame o s        = { s | game         = parseGame o }
-- }}}

-- ** {{{ Lounge --------------------------------------------------
parseLoungeData o =
    { idle  = parseNicks     <| "idle"  .: o
    , games = parseGameInfos <| "games" .: o }

parseNicks : Value -> Set.Set String
parseNicks = Set.fromList << withArray parseString

parseGameInfos : Value -> [GameInfo]
parseGameInfos = withArray (\(Object o) -> parseGame o)

parseGame o = { ident   = parseInt    <| "ident"   .: o
              , topic   = parseString <| "topic"   .: o
              , players = parseNicks  <| "players" .: o }
-- }}}

-- ** {{{ GameEvents ------------------------------------------------
parseGameEvent : Value -> GameEvent
parseGameEvent (Object o) = case "event" .: o |> parseString of
    "round-begin"  -> RoundPrivateStarts  <| parseRoundState o
    "wait-shout"   -> RoundPrivateWaitForShout      <| { seconds = "seconds" .: o |> parseInt
                                                       , shouts  = "shouts"  .: o |> withArray parseShout }
    "wait-turn"    -> RoundPrivateWaitForTurnAction <| hasPlayer o { seconds    = "seconds"     .: o |> parseInt
                                                                   , riichiWith = "riichi-with" .: o |> withArray parseTile
                                                                   }
    "my-hand"      -> RoundPrivateChange  { hand    = "hand"   .: o |> parseHand }
    "turn-changed" -> RoundTurnBegins     <| hasPlayerKaze o { }
    "turn-action"  -> RoundTurnAction     <| hasPlayerKaze o { action  = "action" .: o |> parseTurnAction }
    "shout"        -> RoundTurnShouted    <| hasPlayerKaze o { shout   = "shout"  .: o |> parseShout }
    "hand"         -> RoundHandChanged    <| hasPlayerKaze o { hand    = "hand"   .: o |> parsePublicHand }
    "filpped-dora" -> RoundFlippedDora    { tile = parseTile <| "tile" .: o }
    "nick"         -> RoundNick           <| hasPlayerKaze o { nick    = "nick"   .: o |> parseString }
    "riichi"       -> RoundRiichi         <| hasPlayerKaze o {}
    "end"          -> RoundEnded          <| fromJust <| parseResults <| "results" .: o
    "set-points"   -> RoundGamePoints     <| hasPlayer o { points = "points" .: o |> parseInt }
-- }}}

-- * {{{ Hand -------------------------------------------------------
parseHand : Value -> Hand
parseHand v =
  let x = parsePublicHand v
  in case v of
    Object o ->
        { concealed   = "concealed" .: o |> withArray parseTile
        , pick        = "pick"      .: o |> parseTileMaybe
        , furiten     = "furiten"   .: o |> parseBoolMaybe
        , canTsumo    = "can-tsumo" .: o |> parseBool
        , called      = x.called
        , discards    = x.discards
        , riichi      = x.riichi }

parsePublicHand : Value -> HandPublic
parsePublicHand (Object o) = 
    { called      = "called"       .: o |> withArray parseMentsu
    , discards    = "discards"     .: o |> withArray parseDiscard
    , riichi      = "riichi"       .: o |> parseBool }

parsePlayerHand : Value -> (Kaze, HandPublic)
parsePlayerHand (Array [a, b]) = (parseKaze a, parsePublicHand b)

parseDiscard : Value -> Discard
parseDiscard (Object o) =
   let player = case "to" .: o of
         Null -> Nothing
         String s -> readKaze s |> Just
      in Discard ("tile" .: o |> parseTile) player ("riichi" .: o |> parseBool)
-- }}}

-- * {{{ Mentsu ------------------------------------------------------
parseMentsu : Value -> Mentsu
parseMentsu (Object o) = Mentsu
   (parseMentsuKind <| "kind"  .: o)
   (parseTile       <| "tile"  .: o)
   (parseShoutMaybe <| "shout" .: o)

parseMentsuKind : Value -> MentsuKind
parseMentsuKind (String s) = case s of
   "shuntsu" -> Shuntsu
   "koutsu"  -> Koutsu
   "kantsu"  -> Kantsu
   "jantou"  -> Jantou
-- }}}

-- * {{{ Shout -------------------------------------------------------
parseShout : Value -> Shout
parseShout (Object o) =
   { shoutKind = "kind" .: o |> parseShoutKind
   , shoutFrom = "from" .: o |> parseKaze
   , shoutTile = "tile" .: o |> parseTile
   , shoutTo   = "to"   .: o |> withArray parseTile }

parseShoutKind : Value -> ShoutKind
parseShoutKind (String s) = case s of
   "pon" -> Pon
   "kan" -> Kan
   "chi" -> Chi
   "ron" -> Ron

parseShoutMaybe : Value -> Maybe Shout
parseShoutMaybe v = case v of
   Null     -> Nothing
   Object _ -> Just <| parseShout v
-- }}}

-- * {{{ RoundState -------------------------------------------------------
-- parseRoundState : Value -> RoundState
parseRoundState game = 
    { mypos     = "mypos"      .: game |> parseKaze
    , player    = "player"     .: game |> parseInt
    , myhand    = "myhand"     .: game |> parseHand
    , hands     = "hands"      .: game |> withArray parsePlayerHand
    , round     = "round"      .: game |> parseKaze
    , deal      = "deal"       .: game |> parseInt
    , turn      = "turn"       .: game |> parseKaze
    , oja       = "oja"        .: game |> parseInt
    , firstoja  = "first-oja"  .: game |> parseInt
    , tilesleft = "tiles-left" .: game |> parseInt
    , dora      = "dora"       .: game |> withArray parseTile
    , players   = "players"    .: game |> withArray parsePlayers
    , honba     = "honba"      .: game |> parseInt
    , inTable   = "in-table"   .: game |> parseInt
    , results   = "results"    .: game |> parseResults
    , prevDeals = [] -- TODO : "prev-deals" .: game |> withArray
    }

parsePoints : Value -> (Kaze, Int)
parsePoints (Array [a, b]) = (parseKaze a, parseInt b)

parsePlayers : Value -> (Kaze, (Player, Int, String))
parsePlayers (Array [p, Array [k, ps, n]]) =
    (parseKaze k, (parseInt p, parseInt ps, parseString n))
-- }}}

-- * {{{ Results -------------------------------------------------------
parseResults : Value -> Maybe RoundResult
parseResults val = case val of
    Array [String t, Object o] -> Just <| case t of
      "tsumo" -> DealTsumo { winners = ("winners" .: o |> withArray parseWinner)
                           , payers  = ("payers"  .: o |> withArray parsePayer) }
      "ron"   -> DealRon   { winners = ("winners" .: o |> withArray parseWinner)
                           , payers  = ("payers"  .: o |> withArray parsePayer) }
      "draw"  -> DealDraw  { tenpai  = ("tenpais" .: o |> withArray parsePayer) -- (player, points)
                           , nooten  = ("nooten"  .: o |> withArray parsePayer) }
    Null -> Nothing

parseWinner : Value -> Winner
parseWinner (Array [p, points, h]) = Winner (parsePlayer p) (parseInt points) (parseValuedHand h)

parsePayer : Value -> Payer
parsePayer (Array [p, v]) = Payer (parsePlayer p) (parseInt v)
-- }}}

-- {{{ * Value
parseHandValue : Value -> HandValue
parseHandValue (Object o) = HandValue
   ("yaku"  .: o |> withArray parseYaku)
   ("fu"    .: o |> parseInt)
   ("han"   .: o |> parseInt)
   ("value" .: o |> parseInt)
   ("named" .: o |> parseStringMaybe)

parseValuedHand : Value -> Valued
parseValuedHand (Object o) = Valued
   ("mentsu" .: o |> withArray parseMentsu)
   ("tiles"  .: o |> withArray parseTile)
   ("value"  .: o |> parseHandValue)

parseYaku : Value -> Yaku
parseYaku (Object o) = Yaku
   ("han"  .: o |> parseInt)
   ("name" .: o |> parseString)
-- }}}

-- * {{{ TurnAction -------------------------------------------------------
parseTurnAction : Value -> TurnAction
parseTurnAction (Object o) = case "type" .: o |> parseString of
    "draw"       -> TurnTileDraw    ("wanpai" .: o |> parseBool)
                                    ("tile"   .: o |> parseTileMaybe)
    "discard"    -> TurnTileDiscard (parseDiscard (Object o))
    "ankan"      -> TurnAnkan       ("tile"   .: o |> parseTile)
    "shouminkan" -> TurnShouminkan  ("tile"   .: o |> parseTile)
-- }}}
