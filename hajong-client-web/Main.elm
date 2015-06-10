module Main where

import Lounge
import Game
import Events
import Util
import GameTypes exposing (..)
import JSON exposing (decodeEvent, encodeEvent)

import Set
import Mouse
import Text
import Time
import Json.Decode exposing (Value)
import Graphics.Input.Field as Field
import Graphics.Element exposing (..)
import Color exposing (..)
import Signal exposing (..)

-- {{{ Log view ---------------------------------------------------
logView : GameState -> Element
logView game = container 500 200 topLeft
      <| titled "Log"
      <| flow down
      <| List.map (eventView >> leftAligned)
      <| List.take 6 game.eventlog

titled : String -> Element -> Element
titled str = above (leftAligned <| Text.color charcoal <| Text.fromString str)

eventView : Event -> Text.Text
eventView ev = case ev of
    Identity {nick}        -> "I am `" ++ nick ++ "'"    |> Text.fromString |> Text.color blue
    JoinServer {nick}      -> nick ++ " joined server."  |> Text.fromString |> Text.color green
    PartServer {nick}      -> nick ++ " has left."       |> Text.fromString |> Text.color red
    Message {from,content} -> Text.fromString "<" ++ (Text.fromString from |> Text.bold) ++ Text.fromString "> " ++ Text.fromString content
    Invalid {content}      -> content                    |> Text.fromString |> Text.color white
    LoungeInfo {lounge}    -> Text.fromString "Lounge updated - connection established!" |> Text.color blue
    GameCreated {game}     -> Text.join (Text.fromString " ")
       [ Text.fromString "Game"
       , Text.fromString game.topic
       , Text.fromString <| toString game.ident
       , Text.join (Text.fromString " ") <| List.map Text.fromString <| Set.toList game.players]
    InGameEvents _         -> "" |> Text.fromString
    _                      -> toString ev |> Text.fromString |> Text.color orange
-- }}}

-- {{{ Server IO ----------------------------------------------
port downstream : Signal String

eventInput : Signal Event
eventInput = decodeEvent <~ downstream

-- upstream signal (further handled from JS)
port upstream : Signal String
port upstream = encodeEvent <~ mergeMany
    [ Lounge.events
    , Game.events
    ]
-- }}}

-- {{{ Default state ------------------------------------------
newState : GameState
newState = { status     = InLounge
           , mynick     = ""
           , myid       = 0
           , lounge     = defaultLounge
           , gameWait   = Nothing
           , updated    = 0

           , roundState = Nothing
           , waitTurnAction = Nothing
           , waitShout  = Nothing
           , turnBegan  = 0
           , riichiWith = []

           , eventlog   = []
           , debuglog   = []
           }
-- }}}

-- {{{ Input --------------------------------------------------
type Input = AnEvent Event
           | GameInput Game.Controls
           | LoungeInput Lounge.Controls
           | TimeDelta Time.Time

input = mergeMany
   [ AnEvent <~ eventInput
   , GameInput <~ Game.controls
   , LoungeInput <~ Lounge.controls
   , TimeDelta <~ Time.every Time.second ]
-- }}}

-- {{{ State --------------------------------------------------
gameState : Signal GameState
gameState = foldp stepGame newState input

stepGame : Input -> GameState -> GameState
stepGame x gs = case x of
   AnEvent event -> stepEvent event <| { gs | eventlog <- event :: gs.eventlog }
   TimeDelta time -> { gs | updated <- time
                     , turnBegan <- if gs.turnBegan == 0 then time else gs.turnBegan
                     }
   _             -> gs

-- | Apply an Event to the GameState.
stepEvent : Event -> GameState -> GameState
stepEvent event gameState = case event of
   Identity   {nick}   -> { gameState | mynick <- nick }
   JoinServer {nick}   -> { gameState | lounge <- addIdle nick gameState.lounge }
   PartServer {nick}   -> { gameState | lounge <- deleteNick nick gameState.lounge }
   -- Message {from,content} ->
   -- Invalid {content} ->
   LoungeInfo {lounge} -> { gameState | lounge <- lounge }
   GameCreated {game}  -> { gameState | lounge <- addGame game gameState.lounge }
   JoinGame {nick, ident} ->
       { gameState | lounge   <- addJoinedGame ident nick gameState.lounge
                   , gameWait <-
                       if gameState.mynick == nick
                           then Just ident
                           else gameState.gameWait
       }
   InGameEvents events -> List.foldl Game.processInGameEvent gameState events
   _ -> gameState
-- }}}

-- {{{ Nick and game fiddling ---------------------------------
addGame g l = { l | games <- l.games ++ [g] }
addIdle n l = { l | idle  <- Set.insert n l.idle }

addJoinedGame i n l =
    { l | games <- List.map (\g -> if g.ident == i then { g | players <- Set.insert n g.players } else g) l.games
        , idle  <- Set.remove n l.idle }

deleteNick : String -> LoungeData -> LoungeData
deleteNick n l =
   { l | idle  <- Set.remove n l.idle
       , games <- List.map (\g -> { g | players <- Set.remove n g.players }) l.games }
-- }}}

-- {{{ Display ------------------------------------------------
display : GameState -> Element -> Element
display game view = flow down
   [ view
   , logView game
   ]

mainView = mergeMany
   [ sampleOn (filter Util.atLounge newState gameState)
      (map2 Lounge.display Lounge.controls gameState)
   , sampleOn (filter Util.inGame newState gameState)
      (map2 Game.display Game.controls gameState)
   ]
-- }}}

-- main -------------------------------------------------------
main = map2 display gameState mainView
