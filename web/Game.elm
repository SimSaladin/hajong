module Game where

import Mouse
import Window
import Set (Set)
import Set
import Maybe (..)
import Json
import Graphics.Input (..)
import Graphics.Input.Field as Field

import Connection as Conn
import GameTypes (..)
import Display (..)
import State (..)

-- User Input ----------------------------------------------------------------

data GameInput = InputDelta { userInput : UserInput }
               | InputEvent Conn.Event

type UserInput = { mousePos : (Int,Int)
                 , newTopic : String
                 , doSubmit : Bool
                 }

userInput : Signal UserInput
userInput = UserInput <~ Mouse.position
                       ~ (.string <~ uiInputs.topic.signal)
                       ~ submitted

-- Interactive UI elements

data ButtonState = Submit | Clear

uiInputs = { topic  = input Field.noContent
           , submit = input Clear
           }

topicField : Signal Element
topicField = Field.field Field.defaultStyle uiInputs.topic.handle id "Topic" <~ uiInputs.topic.signal

submitButton : Signal Element
submitButton = constant <| button uiInputs.submit.handle Submit "Create"

newGameForm : Signal Element
newGameForm = flow down <~ combine [topicField, submitButton]

submitted : Signal Bool
submitted = (\x -> x == Submit) <~ uiInputs.submit.signal


-- Part 3: Update the game ---------------------------------------------------

stepGame : GameInput -> GameState -> GameState
stepGame gameInput gameState = case gameInput of

    InputDelta {userInput} -> (\g -> { g | mousepos <- userInput.mousePos }) gameState

    InputEvent event -> Conn.processEvent event
        { gameState | eventlog <- event :: gameState.eventlog }

-- Part 4: Display the game --------------------------------------------------

display : (Int,Int) -> StateView -> Element
display (w,h) view = container w h middle <| case view of
                         Lounge o -> displayLounge o

displayLounge : Viewed AtLounge -> Element
displayLounge o = flow down
        [ container 600 300 midTop    <| loungeView o
        , container 600 300 midBottom <| logView o
        ]


-- Put it all together -------------------------------------------------------

delta = constant 5

gameInput : Signal GameInput
gameInput = merge
    (InputEvent <~ Conn.connect upstream)
    (lift (\x -> InputDelta { userInput = x}) userInput)

upstream : Signal Conn.Event
upstream = merges
    [ constant (Conn.joinServer "mynick") -- TODO some else nick
    , Conn.createGame . .string  <~ sampleOn submitted uiInputs.topic.signal
    ]

gameState : Signal GameState
gameState = foldp stepGame defaultGame gameInput

gameUI : Signal StateView
gameUI = merges
    [ sampleOn loungeState ((\e s -> Lounge { game = s, newgame = e }) <~ newGameForm ~ gameState)
    ]

loungeState : Signal GameState
loungeState = keepIf (\s -> s.status == InLounge) defaultGame gameState

main = lift2 display Window.dimensions gameUI
