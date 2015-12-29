module GameTypes where

import Time exposing (Time)
import Dict
import Set exposing (Set)
import Set
import Debug

-- {{{ GameState -------------------------------------------------------------
type alias GameState =
   { status     : Status
   , mynick     : String
   , myid       : Int
   , lounge     : LoungeData
   , gameWait   : Maybe Int
   , updated    : Time

    -- In-Game
   , roundState     : Maybe RoundState
   , waitTurnAction : Maybe WaitRecord
   , waitShout      : Maybe (WaitRecord, List Shout)
   , turnBegan      : Time
   , riichiWith     : List Tile

   -- Debug
   , logging    : List LogItem
   }

type Status = InLounge | InGame

type alias WaitRecord = { seconds : Int, added : Time }

type LogItem = LogMsg { player_nick : String, msg : String }
             | LogDebug { msg : String }
             | LogError { msg : String }

-- }}}

-- {{{ RoundState ------------------------------------------------------------

type alias Player = Int

-- This duplicates Hajong.Game.Round.GamePlayer
type alias RoundState = 
   { mypos     : Kaze
   , round     : Round
   , turn      : Kaze
   , player    : Player
   , oja       : Player
   , firstoja  : Player
   , tilesleft : Int
   , dora      : List Tile
   , hands     : List (Kaze, HandPublic)
   , players   : List (Kaze, (Player, Int, String))
   , myhand    : Hand
   , results   : Maybe RoundResult
   , honba     : Int
   , inTable   : Int -- Number of riichi sticks in table
   , prevDeals : List Round
   }

type alias Round = { kaze : Kaze, round_rot : Int, round_honba : Int }

type RoundResult = DealTsumo { winners : List Winner, payers : List Payer }
                 | DealRon   { winners : List Winner, payers : List Payer }
                 | DealDraw  { tenpai  : List Payer,  nooten : List Payer }

type alias Winner = { player_kaze : Kaze, points : Points, valuehand : Valued }
type alias Payer  = { player_kaze : Kaze, points : Points }

type alias Points = Int
type alias Yaku = { han : Int, name : String }
type alias Fu = Int
type alias Han = Int

type alias Valued = { mentsu : List Mentsu, tiles : List Tile, value : HandValue }

type alias HandValue =
   { yaku : List Yaku
   , fu : Fu
   , han : Han
   , points : Points
   , named : Maybe String }

-- }}}

-- {{{ Event -----------------------------------------------------------------
type Event = JoinServer  { nick : String, ident : Int }
           | PartServer  { nick : String }
           | Identity    { nick : String }
           | Message     { from : String, content : String } -- ^ from, content
           | JoinGame    { ident : Int, nick : String }

           -- FROM server only
           | Invalid     { content : String }
           | GameCreated { game : GameInfo }
           | InGameEvents (List GameEvent)
           | LoungeInfo  { lounge : LoungeData }

            -- To server only
           | ForceStart  { ident : Int }
           | InGameAction GameAction
           | Noop -- TODO work around elm WS lib signal limitations

type GameEvent = RoundPrivateStarts            RoundState
               | RoundPrivateWaitForShout      { seconds : Int, shouts : List Shout }
               | RoundPrivateWaitForTurnAction { player : Player, seconds : Int, riichiWith : List Tile}
               | RoundPrivateChange            { hand : Hand }
               | RoundTurnBegins               { player_kaze : Kaze }
               | RoundTurnAction               { player_kaze : Kaze, action : TurnAction }
               | RoundTurnShouted              { player_kaze : Kaze, shout : Shout }
               | RoundHandChanged              { player_kaze : Kaze, hand : HandPublic }
               | RoundEnded                    RoundResult
               | RoundNick                     { player_kaze : Kaze, nick : String }

               | RoundFlippedDora              { tile : Tile }
               | RoundRiichi                   { player_kaze : Kaze }
               | RoundGamePoints               { player_kaze : Kaze, points : Points }
-- }}}

-- {{{ Actions ---------------------------------------------------------------
type TurnAction = TurnTileDiscard Discard
                | TurnTileDraw Bool (Maybe Tile) -- ^ From wanpai? - sensitive!
                | TurnAnkan Tile
                | TurnShouminkan Tile
                | TurnTsumo

type GameAction = GameTurn TurnAction
                | GameShout Shout
                | GameDontCare -- ^ About shouting last discarded tile
-- }}}

-- {{{ Lounge ----------------------------------------------------------------
type alias LoungeData =
   { idle : Set String
   , games : List GameInfo }

type alias GameInfo =
   { ident : Int
   , topic : String
   , players : Set String }

defaultLounge = { idle = Set.empty, games = [] }
-- }}}

-- {{{ Tiles -----------------------------------------------------------------
type Tile   = Suited Suit Int Bool | Honor Honor
type Suit   = ManTile | PinTile | SouTile
type Honor  = Kazehai Kaze | Sangenpai Sangen
type Kaze   = Ton | Nan | Shaa | Pei
type Sangen = Haku | Hatsu | Chun

readKaze x = case x of
    "Ton"  -> Ton
    "Nan"  -> Nan
    "Shaa" -> Shaa
    "Pei"  -> Pei
    _      -> Debug.crash <| "Couldn't deserialize string `" ++ x ++ "' to a kaze"

tileOrder : Tile -> Tile -> Order
tileOrder a b = case (a, b) of
   (Suited _ _ _, Honor _) -> LT
   (Honor _, Suited _ _ _) -> GT
   (Suited s n _, Suited t m _) -> case suitOrder s t of
      EQ -> compare n m
      x  -> x
   (Honor (Kazehai _), Honor (Sangenpai _))   -> LT
   (Honor (Sangenpai _), Honor (Kazehai _))   -> GT
   (Honor (Kazehai k), Honor (Kazehai l))     -> kazeOrder k l
   (Honor (Sangenpai s), Honor (Sangenpai t)) -> sangenOrder s t

kazeOrder a b = compare (kazeNth a) (kazeNth b)
sangenOrder a b = compare (sangenNth a) (sangenNth b)
suitOrder a b = compare (suitNth a) (suitNth b)

suitNth s = case s of
   ManTile -> 1
   PinTile -> 2
   SouTile -> 3

kazeNth : Kaze -> Int
kazeNth k = case k of
   Ton  -> 0
   Nan  -> 1
   Shaa -> 2
   Pei  -> 3

sangenNth k = case k of
   Haku  -> 0
   Hatsu -> 1
   Chun  -> 2

isAka : Tile -> Bool
isAka t = case t of
   Suited _ _ a -> a
   _            -> False

sortTiles = List.sortWith tileOrder
-- }}}

-- {{{ Hands -----------------------------------------------------------------
type alias Hand = HandPublic'
   { concealed : List Tile
   , picks     : List PickedTile
   , furiten   : FuritenState
   , canTsumo  : Bool }

type alias HandPublic = HandPublic' {}

type alias HandPublic' a =
   { a
   | state       : DrawState
   , called      : List Mentsu
   , discards    : List Discard
   , riichiState : RiichiState
   , ippatsu     : Bool }

type alias Discard =
   { tile   : Tile
   , to     : Maybe Kaze
   , riichi : Bool }

type RiichiState = NoRiichi | Riichi | DoubleRiichi

type DrawState = DrawFromWanpai | DrawFromWall | DrawNone

type PickedTile = FromWall (Maybe Tile)
                | FromWanpai (Maybe Tile)
                | AgariTsumo Tile
                | AgariTsumoWanpai Tile
                | AgariCall Shout

type FuritenState = NotFuriten | Furiten | TempFuriten

-- }}}

-- {{{ Mentsu ----------------------------------------------------------------
type MentsuKind   = Shuntsu | Koutsu | Kantsu | Jantou
type alias Mentsu =
   { mentsuKind : MentsuKind
   , tiles      : List Tile
   , from       : Maybe Shout }
-- }}}

-- {{{ Shouts ----------------------------------------------------------------
type ShoutKind   = Pon | Kan | Chi | Ron
type alias Shout =
   { shoutKind : ShoutKind
   , shoutFrom : Kaze
   , shoutTile : Tile
   , shoutTo   : List Tile }
-- }}}
