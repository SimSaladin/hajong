module Main where

import Set
import Mouse
import Text
import Graphics.Input.Field as Field

import Connection (..)
import Lounge as Lounge
import Game as Game
import State (..)

data GameInput = InputDelta { userInput : UserInput }
               | InputEvent Event

type UserInput = { mousePos  : (Int,Int)
                 , loungeView : Lounge.View
                 }

userInput : Signal UserInput
userInput = UserInput <~ Mouse.position ~ Lounge.view

-- view: log --------------------------------------------------

logView : GameState -> Element
logView game = above (asText game.debuglog) <| flow down <| map eventView game.eventlog

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
    (InputEvent <~ connect upstream)
    (lift (\x -> InputDelta { userInput = x}) userInput)

-- main state and logic ---------------------------------------

gameState : Signal GameState
gameState = foldp stepGame defaultGame gameInput

stepGame : GameInput -> GameState -> GameState
stepGame gameInput game = case gameInput of
    InputDelta {userInput} -> game |>
        (\g -> { g | mousepos <- userInput.mousePos
                   , debuglog <- show game.gameWait
               })
    InputEvent event       -> processEvent event
        { game | eventlog <- event :: game.eventlog }

-- upstream signal --------------------------------------------

upstream : Signal Event
upstream = merges
    [ joinServer <~ mynick
    , Lounge.events
    ]

-- main display -----------------------------------------------

display : GameState -> Element -> Element
display game view = flow down
   [ container 600 300 midTop view
   , container 600 300 midBottom <| logView game
   ]

-- main -------------------------------------------------------

main = lift2 display gameState <| merges
   [ sampleOn (keepIf atLounge defaultGame gameState)
      (Lounge.display <~ Lounge.view ~ gameState)
   , sampleOn (keepIf inGame defaultGame gameState)
      (Game.display <~ Game.view ~ gameState)
   ]
