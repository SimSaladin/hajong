module ViewingGame where

import GameTypes exposing (..)
import JSON
import Game
import Main
import MsgDialog

import Signal
import Result
import Graphics.Element exposing (flow, down)

port downstream : Signal String -- ^ from javascript

rawState  = Main.newState
gameState = Signal.map (JSON.decodeRoundState >> setRoundState) downstream

setRoundState mrs = case mrs of
   Ok rs   -> { rawState | roundState = Just rs }
   Err err -> { rawState | logging = [ LogError { msg = err } ] }

display gco gs = flow down [ Game.display gco gs]

main = Signal.map2 display Game.controls gameState
