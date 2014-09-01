module Game where

import Mouse
import Window
import Set (Set)
import Set
import Maybe (..)
import Json
-- import Error
import Graphics.Input (..)
import Graphics.Input.Field as Field

import Connection as Conn
import GameTypes (..)
import Display (..)
import State (..)

-- User Input ----------------------------------------------------------------

data GameInput = InputDelta { userInput : UserInput }
               | InputEvent Conn.Event

type UserInput = { mousePos  : (Int,Int)
                 , newTopic  : String
                 , doSubmit  : Bool
                 , gameHover : ChooseGame
                 }

userInput : Signal UserInput
userInput = UserInput <~ Mouse.position
                       ~ (.string <~ uiInputs.topic.signal)
                       ~ submitted
                       ~ uiInputs.choose.signal

-- Interactive UI elements

data ButtonState = Submit | Clear

type ChooseGame = Maybe GameInfo

uiInputs = { topic  = input Field.noContent
           , submit = input Clear
           , choose = input Nothing
           , join   = input Clear
           }

topicField : Signal Element
topicField = Field.field Field.defaultStyle uiInputs.topic.handle id "Topic" <~ uiInputs.topic.signal

joinButton : Element
joinButton = button uiInputs.join.handle Submit "Join"

submitButton : Signal Element
submitButton = constant <| button uiInputs.submit.handle Submit "Create"

newGameForm : Signal Element
newGameForm = flow down <~ combine [topicField, submitButton]

submitted : Signal Bool
submitted = (\x -> x == Submit) <~ uiInputs.submit.signal

joined : Signal (Maybe GameInfo)
joined = sampleOn ((\x -> x == Submit) <~ uiInputs.join.signal) uiInputs.choose.signal

-- Lounge UI

loungeUI : Signal (Viewed AtLounge)
loungeUI = buildLoungeUI <~ keepIf atLounge defaultGame gameState
                          ~ newGameForm

gameListElement : GameInfo -> Element -> Element
gameListElement game = clickable uiInputs.choose.handle (Just game)

buildLoungeUI : GameState -> Element -> Viewed AtLounge
buildLoungeUI game e = { game = game
                       , gamelist = buildGameList game.gameSel game.lounge.games
                       , newgame = e
                       , doJoin = joinButton
                       }

buildGameList : Maybe GameInfo -> [GameInfo] -> [Element]
buildGameList act =
    let active gi = if act == Just gi then color blue else id
    in  map (\gi -> active gi <| gameListElement gi <| gameInfoView gi)

-- Part 3: Update the game ---------------------------------------------------

stepGame : GameInput -> GameState -> GameState
stepGame gameInput game = case gameInput of
    InputDelta {userInput} -> game |>
        (\g -> { g | mousepos <- userInput.mousePos
                   , gameSel  <- userInput.gameHover
                   , debuglog <- show game.gameWait
               })
    InputEvent event       -> Conn.processEvent event
        { game | eventlog <- event :: game.eventlog }

-- Part 4: Display the game --------------------------------------------------

data View = Lounge (Viewed AtLounge)
          | Playing (Viewed {})

display : (Int,Int) -> View -> Element
display (w,h) view = container w h middle <|
    case view of
        Lounge o -> displayLounge o

displayLounge : Viewed AtLounge -> Element
displayLounge o = flow down
        [ container 600 300 midTop    <| loungeView o
        , container 600 300 midBottom <| logView o
        ]

-- Put it all together -------------------------------------------------------

-- delta = constant 5

port mynick : Signal String

gameInput : Signal GameInput
gameInput = merge
    (InputEvent <~ Conn.connect upstream)
    (lift (\x -> InputDelta { userInput = x}) userInput)

upstream : Signal Conn.Event
upstream = merges
    [ Conn.joinServer <~ mynick
    , Conn.createGame . .string  <~ sampleOn submitted uiInputs.topic.signal
    , maybe Conn.Noop (Conn.joinGame . .ident) <~ joined
    ]

gameState : Signal GameState
gameState = foldp stepGame defaultGame gameInput

gameUI : Signal View
gameUI = merges [ Lounge <~ loungeUI ]

main = lift2 display Window.dimensions gameUI
