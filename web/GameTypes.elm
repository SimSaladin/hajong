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

type Tile     = { tileKind : TileKind, number : Int, aka : Bool }
data Kaze     = Ton | Nan | Shaa | Pei
data Sangen   = Haku | Hatsu | Chun
data TileKind = Man | Pin | Sou | Kazehai Kaze | Sangenpai Sangen

-- Hands ---------------------------------------------------------------------

type Hand = { called : [Mentsu], concealed : [Tile] }

type Mentsu     = { mentsuKind : MentsuKind, tile : Tile, from : Maybe (Shout, Kaze, Tile) }
data MentsuKind = Shuntsu | Koutsu | Kantsu | Jantou
data Shout      = Pon | Kan | Chi | Ron

-- Round ---------------------------------------------------------------------

-- This duplicates Hajong.Game.Round.GamePlayer
type RoundState = 
        { mypos     : Kaze
        , round     : Kaze
        , dealer    : Kaze
        , turn      : Kaze
        , dora      : [Tile]
        , tilesleft : Int
        , hands     : Dict.Dict Kaze Hand
        , points    : Dict.Dict Kaze Int
        , results   : Maybe RoundResult
        }

type RoundResult = { endKind : EndKind, winners : [Kaze], payers : [Kaze] }
data EndKind = Tsumo | ByRon | Draw

data TurnAction = TurnTileDiscard Bool Tile -- ^ Riichi?
                | TurnTileDraw Bool (Maybe Tile) -- ^ From wanpai? - sensitive!
                | TurnAnkan Tile

data GameEvent = RoundPrivateStarts {} -- ^ Only at the start of a round
               | RoundPrivateWaitForShout Int -- ^ Number of seconds left to shout or confirm an ignore (See @GameDontCare@)
               | RoundPrivateChange Kaze Hand
               | RoundTurnBegins Kaze
               | RoundTurnAction Kaze TurnAction
               | RoundTurnShouted Kaze Shout -- ^ Who, Shout
               | RoundHandChanged Kaze Hand
               | RoundEnded RoundResult

parseGameEvent o = RoundPrivateStarts {} -- TODO TODO TODO TODO 

data GameAction = GameTurn TurnAction
                | GameShout Shout
                | GameDontCare -- ^ About shouting last discarded tile
