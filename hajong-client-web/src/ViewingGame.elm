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
   Ok rs   -> { rawState | status = InGame rs, rs = rs }
   Err err -> { rawState | logging = [ LogError { msg = err } ] }

display gs = flow down [ Game.display gs ]

main = Signal.map display gameState
