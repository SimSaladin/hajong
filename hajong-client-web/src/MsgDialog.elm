module MsgDialog where

import GameTypes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Text
import Maybe
import Signal
import Keyboard
import Graphics.Input.Field as Field
import Graphics.Element as Element
import Graphics.Element exposing (Element)

dialog : (Int, Int) -> Field.Content -> GameState -> Element
dialog (w, h) content gs = toElement w h <| div [ id "msg-dialog" ]
   [ div [ id "msg-messages" ] <| List.reverse <| List.map dialogObject gs.logging
   , span [ id "msg-dialog-nick" ] [ text gs.mynick ]
   , span [ id "msg-input" ] [ fromElement <| Element.height 30 <| userTextInputField content ]
   , reportBugLink gs
   ]

-- | Msg events
eventMessage : Signal String
eventMessage = Signal.sampleOn
   (Signal.filter identity False Keyboard.enter)
   (Signal.map .string userTextInput.signal)

userTextInput : Signal.Mailbox Field.Content
userTextInput = Signal.mailbox Field.noContent

userTextInputField : Field.Content -> Element
userTextInputField = Field.field Field.defaultStyle (Signal.message userTextInput.address) ""

reportBugLink gs = span [ id "report-bug" ]
                        [ a [ href <| gs.supportURL ++ Maybe.withDefault "" gs.gameUUID
                            , target "_blank" ]
                            [ text "Report a problem in the game" ] ]

dialogObject : LogItem -> Html
dialogObject item = case item of
   LogMsg {player_nick, msg} -> div [ class "msg msg-player" ] [ text (toString player_nick), text msg ]
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
