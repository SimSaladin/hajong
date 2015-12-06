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
import Graphics.Element exposing (..)
import Color exposing (..)
import Graphics.Input exposing (..)
import Graphics.Input.Field as Field

import Html
import Html.Attributes as Html

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
display v gs = doDraw { chosen = v.chosen, game = gs }

doDraw o = flow down
    [ Maybe.withDefault empty <| Maybe.map waitView (o.game.gameWait `Maybe.andThen` Util.lookupGameInfo o.game)
    , blockElement 450 400 (gameListView o) `beside` spacer 5 5 `beside` blockElement 250 400 (idlePlayersView o)
    ]

waitView gameinfo = color lightOrange <| flow right
    [ Text.fromString "Waiting for " |> leftAligned
    , gameInfoView gameinfo
    , forceStartButton gameinfo.ident
    ]

gameListView o = flow down
    [ renderTitle "Public games"
    , spacer 10 10 `beside` (flow down <| buildGameList o.chosen o.game.lounge.games)

    , beside (joinGameButton) <|
      Html.toElement 200 40 <|
         Html.button [ Html.attribute "onclick" "location.replace(\"/new-game\")"
                     , Html.attribute "style" "padding: 0px; margin: 0px; display: block; pointer-events: auto; width: 200px; height: 40px;" ]
                     [ Html.text "Create a new game..." ]
    ]

idlePlayersView o = flow down
   [ renderTitle "Idle players"
   , leftAligned <| Text.fromString (String.join ", " <| Set.toList o.game.lounge.idle)
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
gameListElement game = clickable (chooseGame game)
-- }}}

-- {{{ Force start of the game -------------------------
forceStartMailbox : Mailbox (Maybe Int)
forceStartMailbox = mailbox Nothing

forceStart n = message forceStartMailbox.address (Just n)

forceStartEvent    = maybeEvent Events.forceStart forceStartMailbox.signal
forceStartButton n = button (forceStart n) "Force start"
-- }}}

-- {{{ Join game ---------------------------------------

chosenGame : Mailbox (Maybe GameInfo)
chosenGame = mailbox Nothing
chooseGame game = message chosenGame.address (Just game)

joinGame         = mailbox Clear
joinGameButton   = button (message joinGame.address Submit) "Join"

-- | When joined a game
joined : Signal (Maybe GameInfo)
joined = sampleOn (isSubmit joinGame.signal) chosenGame.signal

-- TODO: game creation inside the elm thing?
-- newGame       = mailbox Clear
-- newGameButton = button (message newGame.address Submit) "New game..."

-- }}}

-- {{{ Helpers -----------------------------------------
isSubmit : Signal ButtonState -> Signal Bool
isSubmit s = map (\x -> x == Submit) s

maybeEvent : (a -> Event) -> Signal (Maybe a) -> Signal Event
maybeEvent f s = map (Maybe.withDefault Noop << Maybe.map f) s
-- }}}
