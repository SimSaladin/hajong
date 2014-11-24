module GameTypes where

import Dict
import Set (Set)
import Set

-- {{{ GameState -------------------------------------------------------------
type GameState = { status     : Status
                 , mynick     : String
                 , lounge     : LoungeData
                 , gameWait   : Maybe Int
                 , updated    : Time

                  -- In-Game
                 , roundState     : Maybe RoundState
                 , waitTurnAction : Maybe WaitRecord
                 , waitShout      : Maybe (WaitRecord, [Shout])
                 , turnBegan      : Time
                 , riichiWith     : [Tile]
                 , canTsumo       : Bool

                 -- Debug
                 , eventlog   : [Event]
                 , debuglog   : [String]
                 }
data Status = InLounge | InGame

type WaitRecord = { seconds : Int, added : Time }
-- }}}

-- {{{ RoundState ------------------------------------------------------------

type Player = Int

-- This duplicates Hajong.Game.Round.GamePlayer
type RoundState = 
        { mypos     : Kaze
        , round     : Kaze
        , turn      : Kaze
        , player    : Player
        , oja       : Player
        , firstoja  : Player
        , tilesleft : Int
        , dora      : [Tile]
        , hands     : [(Kaze, HandPublic)]
        , players   : [(Kaze, (Player, Int, String))]
        , myhand    : Hand
        , results   : Maybe RoundResult
        , actions   : [(Kaze, TurnAction)]
        }

data RoundResult = DealTsumo { winners : [Winner], payers : [Payer] }
                 | DealRon   { winners : [Winner], payers : [Payer] }
                 | DealDraw  { tenpai  : [Player], nooten : [Payer] }

type Winner = { player : Player, valuehand : Valued }
type Payer  = { player : Player, points : Points }

type Points = Int
type Yaku = { han : Int, name : String }
type Fu = Int
type Han = Int

type Valued = { mentsu : [Mentsu], tiles : [Tile], value : HandValue }

type HandValue = { yaku : [Yaku]
               , fu : Fu
               , han : Han
               , points : Points
               , named : Maybe String }
-- }}}

-- {{{ Event -----------------------------------------------------------------
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

data GameEvent = RoundPrivateStarts            RoundState
               | RoundPrivateWaitForShout      { seconds : Int, shouts : [Shout] }
               | RoundPrivateWaitForTurnAction { player : Player, seconds : Int, riichiWith : [Tile]}
               | RoundPrivateChange            { player : Player, hand : Hand }
               | RoundTurnBegins               { player_kaze : Kaze }
               | RoundTurnAction               { player_kaze : Kaze, action : TurnAction }
               | RoundTurnShouted              { player_kaze : Kaze, shout : Shout }
               | RoundHandChanged              { player_kaze : Kaze, hand : HandPublic }
               | RoundEnded                    RoundResult
               | RoundNick                     { player_kaze : Kaze, nick : String }
-- }}}

-- {{{ Actions ---------------------------------------------------------------
data TurnAction = TurnTileDiscard Bool Tile -- ^ Riichi?
                | TurnTileDraw Bool (Maybe Tile) -- ^ From wanpai? - sensitive!
                | TurnAnkan Tile
                | TurnShouminkan Tile

data GameAction = GameTurn TurnAction
                | GameShout Shout
                | GameDontCare -- ^ About shouting last discarded tile
-- }}}

-- {{{ Lounge ----------------------------------------------------------------
type LoungeData = { idle : Set String
                  , games : [GameInfo]
                  }
type GameInfo = { ident : Int, topic : String, players : Set String}

defaultLounge = { idle = Set.empty, games = [] }
-- }}}

-- {{{ Tiles -----------------------------------------------------------------
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

sortTiles = sortWith tileOrder
-- }}}

-- {{{ Hands -----------------------------------------------------------------
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
   }
-- }}}

-- {{{ Mentsu ----------------------------------------------------------------
data MentsuKind = Shuntsu | Koutsu | Kantsu | Jantou
type Mentsu = { mentsuKind : MentsuKind
              , tile       : Tile
              , from       : Maybe Shout
              }
-- }}}

-- {{{ Shouts ----------------------------------------------------------------
data ShoutKind  = Pon | Kan | Chi | Ron
type Shout = { shoutKind : ShoutKind
             , shoutFrom : Kaze
             , shoutTile : Tile
             , shoutTo   : [Tile]
             }
-- }}}
