module GameTypes where

import Dict
import Set (Set)
import Set

type Player = Int

-- GameState -----------------------------------------------------------------
type GameState = { status     : Status
                 , mynick     : String
                 , lounge     : LoungeData
                 , gameWait   : Maybe Int
                 , roundState : Maybe RoundState
                 , eventlog   : [Event]
                 , debuglog   : [String]
                 , waitTurnAction : Maybe Int
                 , waitShout : Maybe Int
                 }
data Status = InLounge | InGame

-- Event ---------------------------------------------------------------------
data Event = JoinServer  { nick : String } -- ^ Nick
           | PartServer  { nick : String }
           | Identity    { nick : String }
           | Message     { from : String, content : String } -- ^ from, content
           | JoinGame    { ident : Int, nick : String }

           -- FROM server only
           | Invalid     { content : String }
           | GameCreated { game : GameInfo }
           | InGameEvents [GameEvent]
           | LoungeInfo  { lounge : LoungeData }

            -- To server only
           | CreateGame String
           | ForceStart  { ident : Int }
           | InGameAction GameAction
           | Noop -- TODO work around elm WS lib signal limitations

data GameEvent = RoundPrivateStarts RoundState
               | RoundPrivateWaitForShout { player : Kaze, seconds : Int }
               | RoundPrivateWaitForTurnAction { player : Kaze, seconds : Int }
               | RoundPrivateChange { player : Kaze, hand : Hand }
               | RoundTurnBegins    { player : Kaze }
               | RoundTurnAction    { player : Kaze, action : TurnAction }
               | RoundTurnShouted   { player : Kaze, shout : Shout }
               | RoundHandChanged   { player : Kaze, hand : HandPublic }
               | RoundEnded         RoundResult

-- Lounge --------------------------------------------------------------------
type LoungeData = { idle : Set String
                  , games : [GameInfo]
                  }
type GameInfo = { ident : Int, topic : String, players : Set String}

defaultLounge = { idle = Set.empty, games = [] }

-- Tiles ---------------------------------------------------------------------
data Tile   = Suited Suit Int Bool | Honor Honor
data Suit   = ManTile | PinTile | SouTile
data Honor  = Kazehai Kaze | Sangenpai Sangen
data Kaze   = Ton | Nan | Shaa | Pei
data Sangen = Haku | Hatsu | Chun

readKaze x = case x of
    "Ton"  -> Ton
    "Nan"  -> Nan
    "Shaa" -> Shaa
    "Pei"  -> Pei

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

kazeNth k = case k of
   Ton  -> 1
   Nan  -> 2
   Shaa -> 3
   Pei  -> 4

sangenNth k = case k of
   Haku  -> 1
   Hatsu -> 2
   Chun  -> 3

sortTiles = sortWith tileOrder

-- Hands ---------------------------------------------------------------------
type Hand = HandPublic' { concealed : [Tile]
                        , pick      : Maybe Tile
                        , furiten   : Maybe Bool
                        }

type HandPublic = HandPublic' {}

type HandPublic' a =
   { a
   | called      : [Mentsu]
   , discards    : [(Tile, Maybe Kaze)]
   , riichi      : Bool
   , turnDiscard : Maybe Tile
   }

data MentsuKind = Shuntsu | Koutsu | Kantsu | Jantou
type Mentsu = { mentsuKind : MentsuKind
              , tile       : Tile
              , from       : Maybe Shout
              }

data ShoutKind  = Pon | Kan | Chi | Ron
type Shout = { shoutKind : ShoutKind
             , shoutFrom : Kaze
             , shoutTile : Tile
             , shoutTo   : [Tile]
             }

-- Round ---------------------------------------------------------------------
-- This duplicates Hajong.Game.Round.GamePlayer
type RoundState = 
        { mypos     : Kaze
        , myhand    : Hand
        , round     : Kaze
        , oja       : Player
        , firstoja  : Player
        , turn      : Kaze
        , dora      : [Tile]
        , tilesleft : Int
        , hands     : [(Kaze, HandPublic)]
        , points    : [(Kaze, Int)]
        , players   : [(Kaze, Int)]
        , results   : Maybe RoundResult
        , actions   : [(Kaze, TurnAction)]
        }

type RoundResult = { endKind : EndKind, winners : [Kaze], payers : [Kaze] }
data EndKind     = Tsumo | ByRon | Draw

-- Actions -------------------------------------------------------------------
data TurnAction = TurnTileDiscard Bool Tile -- ^ Riichi?
                | TurnTileDraw Bool (Maybe Tile) -- ^ From wanpai? - sensitive!
                | TurnAnkan Tile

data GameAction = GameTurn TurnAction
                | GameShout Shout
                | GameDontCare -- ^ About shouting last discarded tile
