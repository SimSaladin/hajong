module Main where

{-| The main on the play site. Unless in a game, show the lounge.

Some debug wirings is also done here. Perhaps that should be put elsewhere
-}

import Http

import Model exposing (GameState, Status(..))
import PlayerInfo
import Lounge
import MsgDialog
import Game
import Events
import Util exposing (..)
import GameTypes exposing (..)
import JSON exposing (decodeEvent, encodeEvent, encodeRoundState)

import View exposing (..)

import Set
import Mouse
import Text
import Time
import Dict
import Graphics.Input.Field as Field
import Task exposing (Task)
import Json.Decode exposing (Value)
import Graphics.Input.Field as Field
import Graphics.Element exposing (..)
import Color exposing (..)
import Signal exposing (..)
import Window
import Debug

-- | Tasks in this app
port runner : Signal (Task x ())
port runner = MsgDialog.tasks

-- A port exposed out of elm which json-encodes current @roundState@
port spyRoundState : Signal String
port spyRoundState =
   let f {status} = case status of
         InGame rs -> encodeRoundState rs
         _ -> ""
   in Signal.map f gameState

-- {{{ Server IO ----------------------------------------------

-- Events from server via ws (supplied in js)
port downstream : Signal String

-- upstream signal: to server. Further handled from JS.
port upstream : Signal String
port upstream = encodeEvent `map` mergeMany
    [ Lounge.events
    , Game.events
    , Signal.map (\msg -> if msg == "" then Noop else Message { from = "", content = msg })
                 MsgDialog.eventMessage
    ]

-- }}}

-- {{{ Profile information

port playerInfoRunner : Signal (Task Http.Error ())
port playerInfoRunner = PlayerInfo.requests <| Signal.map allNicks gameState

-- | Get all nicks in the GameState
allNicks : GameState -> Set.Set String
allNicks gs  = gs.mynick `Set.insert` gs.lounge.idle
     `Set.union` List.foldr (\a b -> Set.union a.players b) Set.empty gs.lounge.games

-- }}}

-- {{{ Game loop input ----------------------------------------

input : Signal Input
input = mergeMany
   [ decodeEvent >> AnEvent  `map` downstream
   , GameInput               `map` Game.userInput
   , LoungeInput             `map` Lounge.userInput
   , MsgDialogInput          `map` MsgDialog.userInput
   , TimeDelta               `map` Time.every Time.second
   , StateUpdate             `map` .signal PlayerInfo.updates
   ]
-- }}}

-- {{{ Sounds -------------------------------------------------
port sounds : Signal String
port sounds = filterMap soundFromInput "" input

soundFromInput : Input -> Maybe String
soundFromInput inp = case inp of
   AnEvent (InGameEvents events) -> soundFromGameEvents events

   _ -> Nothing

soundFromGameEvents : List GameEvent -> Maybe String
soundFromGameEvents events = case events of
   RoundTurnAction {action} :: _   -> soundFromTurnAction action
   _                        :: evs -> soundFromGameEvents evs
   []                              -> Nothing

soundFromTurnAction : TurnAction -> Maybe String
soundFromTurnAction ta = case ta of
   TurnTileDiscard _ -> Just "pop"
   TurnTileDraw _ _  -> Just "pop"
   _                 -> Nothing

-- }}}

-- main -------------------------------------------------------
gameState : Signal GameState
gameState = foldp stepGame newState input
   |> Signal.map2 (\d st -> { st | dimensions = d }) Window.dimensions

main = map display gameState
