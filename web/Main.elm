module Main where

import Lounge
import Game
import Events
import Util
import Connection
import GameTypes (..)

import Set
import Mouse
import Text
import Graphics.Input.Field as Field

data GameInput = InputDelta { userInput : UserInput }
               | InputEvent Event

type UserInput = { mousePos   : (Int,Int)
                 , loungeView : Lounge.View
                 }

userInput : Signal UserInput
userInput = UserInput <~ Mouse.position ~ Lounge.view

-- view: log --------------------------------------------------

logView : GameState -> Element
logView game =
   (flow  down <| map (toText >> Text.color red >> leftAligned) game.debuglog)
   `above` (flow down <| map eventView game.eventlog)

eventView : Event -> Element
eventView ev = case ev of
    JoinServer {nick}      -> nick ++ " joined server." |> toText |> Text.color green |> leftAligned
    PartServer {nick}      -> nick ++ " has left." |> toText |> Text.color red |> leftAligned
    Message {from,content} -> toText "<" ++ (toText from |> bold) ++ toText "> " ++ toText content |> leftAligned
    Invalid {content}      -> toText content |> Text.color white |> leftAligned |> color red
    LoungeInfo {lounge}    -> toText "Lounge updated - connection established!" |> Text.color blue |> leftAligned
    GameCreated {game}     -> join " " ["Game", game.topic, show game.ident, join " " <| Set.toList game.players] |> toText |> leftAligned
    _                      -> asText ev |> color orange

-- view: lounge -----------------------------------------------

-- nick hack --------------------------------------------------
port mynick : Signal String

-- main input -------------------------------------------------
gameInput : Signal GameInput
gameInput = merge
    (InputEvent <~ Connection.connect upstream)
    (lift (\x -> InputDelta { userInput = x}) userInput)

-- start state ------------------------------------------------
defaultGame : GameState
defaultGame = { status = InLounge
              , mynick = "mynick" -- TODO
              , lounge = defaultLounge
              , gameWait = Nothing
              , mousepos = (0,0)
              , eventlog = []
              , debuglog = []
              }

-- logic ------------------------------------------------------
gameState : Signal GameState
gameState = foldp stepGame defaultGame gameInput

stepGame : GameInput -> GameState -> GameState
stepGame gameInput gs = case gameInput of
    InputDelta {userInput} -> { gs | mousepos <- userInput.mousePos }
    InputEvent event       -> stepEvent event { gs | eventlog <- event :: gs.eventlog }

stepEvent : Event -> GameState -> GameState
stepEvent event gameState = case event of
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

addGame g l = { l | games <- l.games ++ [g] }
addIdle n l = { l | idle  <- Set.insert n l.idle }

addJoinedGame i n l =
    { l | games <- map (\g -> if g.ident == i then { g | players <- Set.insert n g.players } else g) l.games
        , idle  <- Set.remove n l.idle }

deleteNick n l =
   { l | idle  <- Set.remove n l.idle
       , games <- map (\g -> { g | players <- Set.remove n g.players }) l.games }

-- upstream signal --------------------------------------------

upstream : Signal Event
upstream = merges
    [ Events.joinServer <~ mynick
    , Lounge.events
    ]

-- main display -----------------------------------------------

display : GameState -> Element -> Element
display game view = flow down
   [ container 800 300 midTop view
   , container 800 300 bottomRight <| logView game
   ]

-- main -------------------------------------------------------

main = lift2 display gameState <| merges
   [ sampleOn (keepIf Util.atLounge defaultGame gameState)
      (Lounge.display <~ Lounge.view ~ gameState)
   , sampleOn (keepIf Util.inGame defaultGame gameState)
      (Game.display <~ Game.view ~ gameState)
   ]
