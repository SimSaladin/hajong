module Main where

import Lounge
import Game
import Events
import Util
import GameTypes (..)
import JSON (fromJSON_Event, toJSON_Event)

import Set
import Mouse
import Text
import Json
import Graphics.Input.Field as Field

-- {{{ Log view ---------------------------------------------------
logView : GameState -> Element
logView game = (
   container 500 200 topLeft
      <| titled "Log"
      <| flow down
      <| map (eventView >> leftAligned)
      <| take 6 game.eventlog
   ) `beside` (
   container 200 200 topLeft
      <| titled "Debug log"
      <| flow down
      <| map (toText >> Text.color red >> leftAligned)
      <| take 6 game.debuglog)

titled : String -> Element -> Element
titled str = above (leftAligned <| Text.color charcoal <| toText str)

eventView : Event -> Text.Text
eventView ev = case ev of
    Identity {nick}        -> "I am `" ++ nick ++ "'"    |> toText |> Text.color blue
    JoinServer {nick}      -> nick ++ " joined server."  |> toText |> Text.color green
    PartServer {nick}      -> nick ++ " has left."       |> toText |> Text.color red
    Message {from,content} -> toText "<" ++ (toText from |> bold) ++ toText "> " ++ toText content
    Invalid {content}      -> content                    |> toText |> Text.color white
    LoungeInfo {lounge}    -> toText "Lounge updated - connection established!" |> Text.color blue
    GameCreated {game}     -> join " " ["Game", game.topic, show game.ident, join " " <| Set.toList game.players] |> toText
    InGameEvents _         -> "" |> toText
    _                      -> show ev |> toText |> Text.color orange
-- }}}

-- {{{ Server IO ----------------------------------------------
port downstream : Signal String

eventInput : Signal Event
eventInput = fromJSON_Event <~ downstream

-- upstream signal (further handled from JS)
port upstream : Signal String
port upstream = (toJSON_Event >> Json.toString "") <~ merges
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
data Input = AnEvent Event
           | GameInput Game.Controls
           | LoungeInput Lounge.Controls
           | TimeDelta Time

input = merges [ AnEvent <~ eventInput
               , GameInput <~ Game.controls
               , LoungeInput <~ Lounge.controls
               , TimeDelta <~ every second
               ]
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
   InGameEvents events -> foldl Game.processInGameEvent gameState events
   _ -> gameState
-- }}}

-- {{{ Nick and game fiddling ---------------------------------
addGame g l = { l | games <- l.games ++ [g] }
addIdle n l = { l | idle  <- Set.insert n l.idle }

addJoinedGame i n l =
    { l | games <- map (\g -> if g.ident == i then { g | players <- Set.insert n g.players } else g) l.games
        , idle  <- Set.remove n l.idle }

deleteNick n l =
   { l | idle  <- Set.remove n l.idle
       , games <- map (\g -> { g | players <- Set.remove n g.players }) l.games }
-- }}}

-- {{{ Display ------------------------------------------------
display : GameState -> Element -> Element
display game view = flow down
   [ view
   , logView game
   ]

mainView = merges
   [ sampleOn (keepIf Util.atLounge newState gameState)
      (lift2 Lounge.display Lounge.controls gameState)
   , sampleOn (keepIf Util.inGame newState gameState)
      (lift2 Game.display Game.controls gameState)
   ]
-- }}}

-- main -------------------------------------------------------
main = lift2 display gameState mainView
