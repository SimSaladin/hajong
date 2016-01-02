module Lounge where

import GameTypes exposing (..)
import Events
import Util exposing (..)

import Set
import Text
import String
import Text exposing (bold)
import Keyboard
import Signal exposing (..)
import Graphics.Element as Element
import Graphics.Element exposing (..)
import Color exposing (..)
import Graphics.Input as Input
import Graphics.Input.Field as Field

import Html exposing (..)
import Html.Attributes exposing (..)

type ButtonState = Submit | Clear

-- {{{ Controls ----------------------------------------
type alias Controls = { chosen : Maybe GameInfo }

controls : Signal Controls
controls = map Controls chosenGame.signal
-- }}}

-- {{{ Events ------------------------------------------
events : Signal Event
events = mergeMany
    [ maybeEvent (Events.joinGame << .ident) joined
    , forceStartEvent ]
-- }}}

-- {{{ Display -----------------------------------------
display : Controls -> GameState -> Element
display co gs = flow down
    [ currentWaitInfo co gs
    , blockElement 450 400 (gameListing co gs) `beside` spacer 5 5 `beside`
      blockElement 250 400 (idlePlayers co gs)
    ]

-- | Waiting for a game to start
currentWaitInfo : Controls -> GameState -> Element
currentWaitInfo co gs = case gs.gameWait `Maybe.andThen` Util.lookupGameInfo gs of
   Nothing       -> empty
   Just gamewait ->
      let info = flow right [ Text.fromString "Waiting for " |> leftAligned, gameInfoView gamewait ]
                  |> container 500 50 middle
                  |> color lightOrange
          btn  = forceStartButton gamewait.ident
                  |> container 150 50 middle
      in info `beside` btn |> container 700 70 middle

gameListing : Controls -> GameState -> Element
gameListing co gs = flow down
    [ renderTitle "Games"
    , spacer 10 10 `beside` (flow down <| buildGameList co.chosen gs.lounge.games)

    , beside (joinGameButton) <|
      toElement 200 40 <|
         button [ attribute "onclick" "location.replace(\"/new-game\")"
                , attribute "style" "padding: 0px; margin: 0px; display: block; pointer-events: auto; width: 200px; height: 40px;" ]
                [ text "Create a new game..." ]
    ]

idlePlayers : Controls -> GameState -> Element
idlePlayers co gs = flow down
   [ renderTitle "Idle players"
   , leftAligned <| Text.fromString (String.join ", " <| Set.toList gs.lounge.idle)
   ]
-- }}}

-- {{{ Game info --------------------------------------

-- | `active_game all_games`
buildGameList : Maybe GameInfo -> List GameInfo -> List Element
buildGameList act =
    let active gi = if act == Just gi then color blue else identity
    in  List.map (\gi -> active gi <| gameListElement gi <| gameInfoView gi)

gameInfoView : GameInfo -> Element
gameInfoView {ident,topic,players} =
    let pc = List.length <| Set.toList players
    in leftAligned <|
        (toString ident |> Text.fromString |> Text.color red) ++
        Text.fromString " " ++ (Text.fromString topic |> bold)  ++ Text.fromString " " ++
        Text.fromString " (" ++ (toString pc |> Text.fromString |> bold) ++ Text.fromString "/4 players)" ++
        Text.fromString " {" ++ Text.fromString (String.join ", " <| Set.toList players) ++ Text.fromString "}"

gameListElement : GameInfo -> Element -> Element
gameListElement game = Input.clickable (chooseGame game)
-- }}}

-- {{{ Force start of the game -------------------------
forceStartMailbox : Mailbox (Maybe Int)
forceStartMailbox = mailbox Nothing

forceStart n = message forceStartMailbox.address (Just n)

forceStartEvent    = maybeEvent Events.forceStart forceStartMailbox.signal
forceStartButton n = Input.button (forceStart n) "Force start"
-- }}}

-- {{{ Join game ---------------------------------------

chosenGame : Mailbox (Maybe GameInfo)
chosenGame = mailbox Nothing
chooseGame game = message chosenGame.address (Just game)

joinGame         = mailbox Clear
joinGameButton   = Input.button (message joinGame.address Submit) "Join"

-- | When joined a game
joined : Signal (Maybe GameInfo)
joined = sampleOn (isSubmit joinGame.signal) chosenGame.signal

-- TODO: game creation inside the elm thing?
-- newGame       = mailbox Clear
-- newGameButton = Input.button (message newGame.address Submit) "New game..."

-- }}}

-- {{{ Helpers -----------------------------------------
isSubmit : Signal ButtonState -> Signal Bool
isSubmit s = map (\x -> x == Submit) s

maybeEvent : (a -> Event) -> Signal (Maybe a) -> Signal Event
maybeEvent f s = map (Maybe.withDefault Noop << Maybe.map f) s
-- }}}
