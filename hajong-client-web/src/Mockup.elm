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
overrideResources = Dict.fromList
   [ ("tiles", "http://localhost:3000/static/img/Mahjong-tiles.jpg") ]

roundState : RoundState
roundState = 
   { mypos         = Ton
   , round         = { kaze = Ton, round_rot = 1, round_honba = 0 }
   , turn          = Ton
   , player        = 0
   , oja           = 0
   , firstoja      = 0
   , tilesleft     = 10
   , dora          = []
   , hands         = List.map2 (,) [Ton, Nan, Shaa, Pei] [publicHand, publicHand, publicHand, publicHand]
   , players       = List.map2 (,) [Ton, Nan, Shaa, Pei] [dummyPlayer, dummyPlayer, dummyPlayer, dummyPlayer]
   , myhand        = { concealed = [Suited ManTile 1 False], picks = [], furiten = NotFuriten,
                        canTsumo = False, state = DrawNone, called = [],
                        discards = [], riichiState = NoRiichi, ippatsu = True }
   , results       = Nothing
   , honba         = 0
   , inTable       = 0
   , eventsHistory = []
   }

dummyPlayer : (Player, Int, String)
dummyPlayer = (0, 20000, "dummy")

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
