module Main where

import Lounge
import MsgDialog
import Game
import Events
import Util exposing (..)
import GameTypes exposing (..)
import JSON exposing (decodeEvent, encodeEvent, encodeRoundState)

import Set
import Mouse
import Text
import Time
import Json.Decode exposing (Value)
import Graphics.Input.Field as Field
import Graphics.Element exposing (..)
import Color exposing (..)
import Signal exposing (..)
import Debug

-- {{{ Server IO ----------------------------------------------
port downstream : Signal String

eventInput : Signal Event
eventInput = decodeEvent `map` downstream

-- upstream signal: to server. Further handled from JS.
port upstream : Signal String
port upstream = encodeEvent `map` mergeMany
    [ Lounge.events
    , Game.events
    , Signal.map (\msg -> if msg == "" then Noop else Message { from = "", content = msg })
                 MsgDialog.eventMessage
    ]

-- A port exposed out of elm which json-encodes current @roundState@
port spyRoundState : Signal String
port spyRoundState =
   let f {roundState} = case roundState of
         Just r  -> encodeRoundState r
         Nothing -> ""
   in Signal.map f gameState

-- }}}

-- {{{ Default state ------------------------------------------
newState : GameState
newState = { status     = InLounge
           , mynick     = ""
           , myid       = 0
           , lounge     = defaultLounge
           , gameWait   = Nothing
           , updated    = 0
           , supportURL = "/support/"

           , roundState = Nothing
           , gameUUID   = Nothing
           , waitTurnAction = Nothing
           , waitShout  = Nothing
           , turnBegan  = 0
           , riichiWith = []

           , logging    = []
           }
-- }}}

-- {{{ Game loop input ----------------------------------------
type Input = AnEvent Event
           | GameInput Game.Controls
           | LoungeInput Lounge.Controls
           | TimeDelta Time.Time

input : Signal Input
input = mergeMany
   [ AnEvent `map` eventInput
   , GameInput `map` Game.controls
   , LoungeInput `map` Lounge.controls
   , TimeDelta `map` Time.every Time.second ]
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

-- {{{ State --------------------------------------------------
gameState : Signal GameState
gameState = foldp stepGame newState input

stepGame : Input -> GameState -> GameState
stepGame x gs = case x of
   AnEvent event -> stepEvent event <| { gs | logging =
      maybe gs.logging (\x -> x :: gs.logging) <| MsgDialog.eventToDebugMsg event }
   TimeDelta time -> { gs | updated = time
                     , turnBegan = if gs.turnBegan == 0 then time else gs.turnBegan
                     }
   _             -> gs

-- | Apply an Event to the GameState.
stepEvent : Event -> GameState -> GameState
stepEvent event gameState = case event of
   Identity   {nick}   -> { gameState | mynick = nick }
   JoinServer {nick}   -> { gameState | lounge = addIdle nick gameState.lounge }
   PartServer {nick}   -> { gameState | lounge = deleteNick nick gameState.lounge }
   Message {from,content} -> gameState -- TODO
   Invalid {content} -> gameState -- TODO
   LoungeInfo {lounge} -> { gameState | lounge = lounge }
   GameCreated {game}  -> { gameState | lounge = addGame game gameState.lounge }
   JoinGame {nick, ident} ->
       { gameState | lounge   = addJoinedGame ident nick gameState.lounge
                   , gameWait = if gameState.mynick == nick then Just ident
                                                            else gameState.gameWait
                   , gameUUID = Maybe.map .uuid <| lookupGameInfo gameState ident
       }
   InGameEvents events -> List.foldl Game.processInGameEvent gameState events
   _ -> gameState
-- }}}

-- {{{ Nick and game fiddling ---------------------------------
addGame g l = { l | games = l.games ++ [g] }
addIdle n l = { l | idle  = Set.insert n l.idle }

addJoinedGame i n l =
    { l | games = List.map (\g -> if g.ident == i then { g | players = Set.insert n g.players } else g) l.games
        , idle  = Set.remove n l.idle }

deleteNick : String -> LoungeData -> LoungeData
deleteNick n l =
   { l | idle  = Set.remove n l.idle
       , games = List.map (\g -> { g | players = Set.remove n g.players }) l.games }
-- }}}

-- {{{ Display ------------------------------------------------
display : Field.Content -> GameState -> Element -> Element
display userinput game view = flow down
   [ view
   , MsgDialog.dialog (500, 200) userinput game
   ]

mainView = mergeMany
   [ sampleOn (filter Util.atLounge newState gameState)
      (map2 Lounge.display Lounge.controls gameState)
   , sampleOn (filter Util.inGame newState gameState)
      (map2 Game.display Game.controls gameState)
   ]
-- }}}

-- main -------------------------------------------------------
main = map3 display (.signal MsgDialog.userTextInput) gameState mainView
