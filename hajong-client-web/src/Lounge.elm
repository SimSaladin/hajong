module Lounge where

import GameTypes exposing (..)
import Events
import Util exposing (..)

import Dict exposing (Dict)
import Dict
import Set
import Text
import String
import Text exposing (bold)
import Keyboard
import Signal exposing (..)
import Graphics.Element exposing (Element, beside, container, flow, down, right, middle, color)
import Graphics.Element as Element
import Color exposing (..)
import Graphics.Input as Input
import Graphics.Input.Field as Field

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type ButtonState = Submit | Clear

-- {{{ Helpers -----------------------------------------
isSubmit : Signal ButtonState -> Signal Bool
isSubmit s = map (\x -> x == Submit) s
-- }}}

-- {{{ State, input and events ------------------------

-- | This is like GameTypes.GameState, but only the fields we actually use in
-- this module.
type alias State a =
   { a | lounge          : LoungeData
       , mynick          : String
       , lobbyChosenGame : Maybe GameInfo
       , profilePictures : Dict String String -- dom. urls
       , gameWait        : Maybe Int
       , dimensions      : (Int, Int)
   }

-- | User input this module listens to.
type UserInput = ChooseGame (Maybe GameInfo)

userInput : Signal UserInput
userInput = ChooseGame `map` chosenGame.signal

-- | Handling user input events in the state.
stepUserInput : UserInput -> State a -> State a
stepUserInput inp st = case inp of
   ChooseGame mg -> { st | lobbyChosenGame = mg }

-- | This module emits modules directly to the game server.
events : Signal Event
events = mergeMany
    [ maybeEvent (Events.joinGame << .ident) <| sampleOn (isSubmit joinGame.signal) chosenGame.signal
    , maybeEvent Events.forceStart forceStartMailbox.signal
    ]
-- }}}

-- {{{ Mailboxes
forceStartMailbox : Mailbox (Maybe Int)
forceStartMailbox = mailbox Nothing

chosenGame : Mailbox (Maybe GameInfo)
chosenGame = mailbox Nothing

joinGame : Mailbox ButtonState
joinGame = mailbox Clear
-- }}}

-- {{{ Buttons -----------------------------------------
chooseGame game = message chosenGame.address (Just game)
forceStart n    = message forceStartMailbox.address (Just n)

joinGameButton     = Input.button (message joinGame.address Submit) "Join"
forceStartButton n = Input.button (forceStart n) "Force start"
-- }}}

-- {{{ Display -----------------------------------------
display : State a -> Element
display st = flow down
    [ currentWaitInfo st
    , toElement (fst st.dimensions) 500 <| div [ class "lobby" ]
                       [ gameListing st
                       , idlePlayers st ]
    ]

idlePlayers : State a -> Html
idlePlayers st = 
   div [ class "idle-players" ]
       [ ul [ class "idle-players-list" ]
            [ toPlayerInfoLi st st.mynick ]
       , h2 [] [text "Players Idle" ]
       , i [] [ text "Not waiting for a game to start" ]
       , ul [ class "idle-players-list" ]
            <| List.map (toPlayerInfoLi st)
              (List.filter (\nick -> nick /= st.mynick) <| Set.toList st.lounge.idle)
       ]

-- | Waiting for a game to start
currentWaitInfo : State a -> Element
currentWaitInfo st = case st.gameWait `Maybe.andThen` Util.lookupGameInfo st of
   Just gamewait ->

      let info = flow right [ Text.fromString "Waiting for " |> Element.leftAligned
                            , toElement 300 50 <| gameInfoBlock st (\_ -> "waiting") gamewait ]
                  |> container 500 50 middle
                  |> color lightOrange

          btn  = forceStartButton gamewait.ident
                  |> container 150 50 middle

      in info `beside` btn |> container 700 70 middle

   Nothing       -> Element.empty

-- {{{ Game list

gameListing : State a -> Html
gameListing st =
   div [ class "games" ]
       [ h2 [] [ text "Games" ]
       , i [] [ text "Choose a game and click Join to to play in it or create a new game." ]
       , buildGameList st
       , fromElement <| joinGameButton `beside` newGameButton
       ]

-- | `active_game all_games`
buildGameList : State a -> Html
buildGameList st =
   let chooseClass gi = case st.gameWait of
                              Nothing  -> if st.lobbyChosenGame == Just gi then "chosen" else "normal"
                              Just gid -> if gid == gi.ident then "waiting" else "normal"

       gameInfoLi gi  = li [ onClick chosenGame.address (Just gi) ]
                           [ gameInfoBlock st chooseClass gi ]

       in ul [ class "games-list" ]
             <| List.map gameInfoLi st.lounge.games

gameInfoBlock : State a -> (GameInfo -> String) -> GameInfo -> Html
gameInfoBlock st getsClass gi =
   div [ class <| "game-info-block " ++ getsClass gi ]
       [ span [ class "game-ident"        ] [ text (toString gi.ident) ]
       , span [ class "game-name"         ] [ text gi.topic ]
       , span [ class "game-player-count" ] [ text <| toString (Set.size
       gi.players) ++ " out of 4 players" ]
       , div  [ class "game-players"      ]
              (Set.toList gi.players |> List.map (smallPlayerInfo st))
       ]

newGameButton : Element
newGameButton = toElement 200 40 <|
   button [ attribute "onclick" "location.replace(\"/new-game\")"
          , attribute "style" "padding: 0px; margin: 0px; display: block; pointer-events: auto; width: 200px; height: 40px;" ]
          [ text "Create a new game..." ]

-- }}}

-- }}}

-- {{{ Display player information

-- | li-element based on nick
toPlayerInfoLi : State a -> String -> Html
toPlayerInfoLi st nick = li []
     [ img [ src <| Maybe.withDefault "" (Dict.get nick st.profilePictures)
           , width 50, height 50 ] []
     , text nick ]

smallPlayerInfo : {a | profilePictures : Dict String String } -> String -> Html
smallPlayerInfo st nick =
   img [ src <| Maybe.withDefault "" (Dict.get nick st.profilePictures)
       , title nick, width 30, height 30
       ] []

-- }}}
