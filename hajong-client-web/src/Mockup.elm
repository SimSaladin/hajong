module Mockup where

import View
import View exposing (newState)

import GameTypes exposing (..)
import JSON
import MsgDialog
import Game
import Lounge

import Signal
import Signal exposing (Signal, map)
import Window
import Time
import List
import Dict

import Debug

statePlaying : RoundState -> GameState
statePlaying rs =
   { newState | status = InGame rs, rs = rs
              , resources = overrideResources `Dict.union` View.defaultResources
   }

overrideResources : Dict.Dict String String
overrideResources = View.defaultResources |> Dict.map (\_ x ->
   "http://localhost:3000" ++ x)

roundState : RoundState
roundState = identity
   -- <| setsDraw
   -- <| setsTsumo
   <| setsRon
   <| setsDefaults
   <| View.emptyRoundState

setsDefaults rs = { rs
   | player        = 0
   , oja           = 0
   , firstoja      = 0
   , tilesleft     = 10
   , dora          = [Suited ManTile 1 False, Suited PinTile 5 True]
   , hands         = List.map2 (,) [Ton, Nan, Shaa, Pei]
      [ publicHand
      , { publicHand | riichiState = Riichi
                     , discards    = [ { tile = Suited PinTile 6 False, to = Nothing, riichi = False } ]
                  }
      , publicHand, publicHand ]
   , players       = List.map2 (,) [Ton, Nan, Shaa, Pei] [dummyPlayer, dummyPlayer, dummyPlayer, dummyPlayer]
   , myhand        = { concealed = [Suited ManTile 1 False], picks = [], furiten = NotFuriten,
                        canTsumo = False, state = DrawNone, called = [],
                        discards = [], riichiState = NoRiichi, ippatsu = True }
   , results       = Nothing
   , honba         = 0
   , inTable       = 0
   , eventsHistory = []
   }

setsTsumo rs = { rs
   | results = Just <| DealTsumo { winners = [winner Ton, winner Pei, winner Shaa], payers = [] }
   }

setsDraw rs = { rs
   | results = Just <| DealDraw { tenpai = [tenpai Nan, tenpai Shaa], nooten = [] }
   }

setsRon rs = { rs
   | results = Just <| DealRon { winners = [winner Shaa], payers = [] }
   }

winner k = { player_kaze = k, points = 5000, valuehand = valued }

tenpai k = { player_kaze = k, points = 1000, mentsu = [], tiles =
   List.repeat 13 (Suited PinTile 6 False) }

valued = { mentsu = [], tiles = List.repeat 6 (Suited PinTile 6 False), value = value }
value = { yaku = [ { han = 1, name = "Pinfu" }, { han = 1, name = "Riichi" }, {
   han = 5, name = "Aoeu-Asdf-Colem-Qwerty" } ]
        , fu = 30, han = 2, points = 9001, named = Just "derp" }

dummyPlayer : (Player, Int, String)
dummyPlayer = (0, 20000, "")

publicHand : HandPublic
publicHand = { state = DrawNone, called = [], discards = [], riichiState = NoRiichi, ippatsu = True }

gameState : Signal GameState
gameState =
   let st = statePlaying roundState
       in Signal.foldp View.stepGame st input

input : Signal View.Input
input = Signal.mergeMany
   -- [ JSON.decodeEvent >> AnEvent              `map` downstream
   [ View.GameInput                                `map` Game.userInput
   , View.LoungeInput                              `map` Lounge.userInput
   , View.MsgDialogInput                           `map` MsgDialog.userInput
   , View.minDimensions >> View.Dimensions         `map` Window.dimensions
   , View.TimeDelta                                `map` Time.every Time.second ]

debug : GameState -> GameState
debug = Debug.watchSummary "Spying" <| \gs -> toString gs.dimensions

main = map View.display (map debug gameState)
