module MsgDialog where

{-| A chat box -}

import GameTypes exposing (..)
import Model exposing (LogItem(..))

import Html exposing (..)
import Html.Attributes exposing (..)

import Text
import Maybe
import Signal
import Keyboard
import Task exposing (Task)
import Graphics.Input.Field as Field
import Graphics.Element as Element
import Graphics.Element exposing (Element)

-- TODO: Should just use the `Model`.
type alias State a = { a | dialogFieldContent : Field.Content
                         , dimensions : (Int, Int)
                         , logging : List LogItem
                         , mynick : String
                         , supportURL : String
                         , gameUUID : Maybe String
                      }

type UserInput = TextInput Field.Content

userInput : Signal UserInput
userInput = TextInput `Signal.map` userTextInput.signal

stepUserInput : UserInput -> State a -> State a
stepUserInput inp st = case inp of
   TextInput content -> { st | dialogFieldContent = content }

-- | Emits Msg events to server
eventMessage : Signal String
eventMessage = Signal.sampleOn
   (Signal.filter identity False Keyboard.enter)
   (Signal.map .string userTextInput.signal)

tasks : Signal (Task x ())
tasks = Signal.sampleOn eventMessage
   (Signal.constant <| Signal.send userTextInput.address Field.noContent)

-- | Display dialog
dialog : State a -> Element
dialog st = toElement (fst st.dimensions) 200 <|
   div [ id "msg-dialog" ]
       [ div [ id "msg-messages" ] <| List.reverse <| List.map dialogObject st.logging
       , div [ id "msg-controls" ]
             [ span [ id "msg-dialog-nick" ]
                    [ text st.mynick ]

             , span [ id "msg-input" ]
                    [ fromElement <| Element.width 500 <| Element.height 40 <| userTextInputField st.dialogFieldContent ]
             , reportBugLink st ]
       ]

------------------------------------------------

userTextInput : Signal.Mailbox Field.Content
userTextInput = Signal.mailbox Field.noContent

userTextInputField : Field.Content -> Element
userTextInputField = Field.field Field.defaultStyle (Signal.message userTextInput.address) ""

reportBugLink : State a -> Html
reportBugLink st = span [ id "report-bug" ]
                        [ a [ href <| st.supportURL ++ Maybe.withDefault "" st.gameUUID
                            , target "_blank" ]
                            [ text "Report a problem in the game" ] ]

dialogObject : LogItem -> Html
dialogObject item = case item of
   LogMsg {player_nick, msg} -> div [ class "msg msg-player" ] [ text <|
   player_nick ++ ": ", text msg ]
   LogDebug {msg}            -> div [ class "msg msg-debug" ] [ text msg ]
   LogError {msg}            -> div [ class "msg msg-error" ] [ text msg ]

eventToDebugMsg : Event -> Maybe LogItem
eventToDebugMsg ev = case ev of
    Identity {nick}        -> Just <| LogDebug {msg = "My nick is `" ++ nick ++ "'"}
    JoinServer {nick}      -> Just <| LogDebug {msg = nick ++ " joined server"}
    PartServer {nick}      -> Just <| LogDebug {msg = nick ++ " left the server"}
    Message {from,content} -> Just <| LogMsg {player_nick = from, msg = content}
    Invalid {content}      -> Just <| LogError {msg = content}
    LoungeInfo {lounge}    -> Just <| LogDebug {msg = "Lounge updated"}
    GameCreated {game}     -> Just <| LogDebug {msg = "Game `" ++ game.topic ++ "' (" ++ toString (game.ident) ++ ") created"}
    JoinGame{nick,ident}   -> Just <| LogDebug {msg = nick ++ " joined game " ++ toString ident }
    InGameEvents _         -> Nothing
    _                      -> Nothing
