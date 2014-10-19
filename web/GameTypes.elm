module GameTypes where

import Dict
import Set (Set)
import Set

-- Lounge --------------------------------------------------------------------

type LoungeData = { idle : Set String
                  , games : [GameInfo]
                  }
type GameInfo = { ident : Int, topic : String, players : Set String}

defaultLounge = { idle = Set.empty, games = [] }

-- Tiles ---------------------------------------------------------------------

data Tile   = Suited Suit Int Bool | Honor Honor
data Suit   = Man | Pin | Sou
data Honor  = Kazehai Kaze | Sangenpai Sangen
data Kaze   = Ton | Nan | Shaa | Pei
data Sangen = Haku | Hatsu | Chun

readKaze x = case x of
    "Ton"  -> Ton
    "Nan"  -> Nan
    "Shaa" -> Shaa
    "Pei"  -> Pei

-- Hands ---------------------------------------------------------------------

type Hand = HandPublic' { concealed : [Tile]
                        , pick      : Maybe Tile
                        , furiten   : Maybe Bool
                        }

type HandPublic = HandPublic' {}

type HandPublic' a =
   { a
   | called      : [Mentsu]
   , discards    : [Tile]
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
        , dealer    : Kaze
        , turn      : Kaze
        , dora      : [Tile]
        , tilesleft : Int
        , hands     : [(Kaze, HandPublic)]
        , players   : [(Kaze, String, Int)]
        , results   : Maybe RoundResult
        }

type RoundResult = { endKind : EndKind, winners : [Kaze], payers : [Kaze] }
data EndKind = Tsumo | ByRon | Draw

data TurnAction = TurnTileDiscard Bool Tile -- ^ Riichi?
                | TurnTileDraw Bool (Maybe Tile) -- ^ From wanpai? - sensitive!
                | TurnAnkan Tile

data GameEvent = RoundPrivateStarts RoundState
               | RoundPrivateWaitForShout { seconds : Int }
               | RoundPrivateChange { player : Kaze, hand : Hand }
               | RoundTurnBegins    { player : Kaze }
               | RoundTurnAction    { player : Kaze, action : TurnAction }
               | RoundTurnShouted   { player : Kaze, shout : Shout }
               | RoundHandChanged   { player : Kaze, hand : Hand }
               | RoundEnded         RoundResult

data GameAction = GameTurn TurnAction
                | GameShout Shout
                | GameDontCare -- ^ About shouting last discarded tile
