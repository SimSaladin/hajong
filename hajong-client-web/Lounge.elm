module Lounge where

import GameTypes exposing (..)
import Events
import Util

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

type ButtonState = Submit | Clear

-- {{{ Controls ----------------------------------------
type alias Controls = { chosen : Maybe GameInfo }

controls : Signal Controls
controls = Controls <~ chosenGame.signal
-- }}}

-- {{{ Events ------------------------------------------
events : Signal Event
events = mergeMany
    [ maybeEvent (Events.joinGame << .ident) joined
    , forceStartEvent ]
-- }}}

-- {{{ Display -----------------------------------------
display : Controls -> GameState -> Element
display v gs = doDraw { v | game = gs }

doDraw o = flow down
    [ Maybe.withDefault
         (spacer 10 10 |> color red)
         <| Maybe.map waitView (o.game.gameWait `Maybe.andThen` Util.lookupGameInfo o.game)

    , Text.fromString "Lounge" |> bold |> centered
    , blockElement 250 400 (gameListView o)
    ]

waitView gameinfo = color lightOrange <| flow right
    [ Text.fromString "Waiting for " |> leftAligned
    , gameInfoView gameinfo
    , forceStartButton gameinfo.ident
    ]

gameListView o = flow down
    [ Text.fromString "Available games" |> bold |> centered
    , flow down   <| buildGameList o.chosen o.game.lounge.games
    , joinGameButton
    , leftAligned <| Text.fromString "Idle players: " `Text.append`
                     Text.fromString (String.join ", " <| Set.toList o.game.lounge.idle)
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

joinGame : Mailbox ButtonState
joinGame = mailbox Clear

joinGameButton : Element
joinGameButton = button (message joinGame.address Submit) "Join"

-- | When joined a game
joined : Signal (Maybe GameInfo)
joined = sampleOn (isSubmit joinGame.signal) chosenGame.signal

-- }}}

-- {{{ Helpers -----------------------------------------
isSubmit : Signal ButtonState -> Signal Bool
isSubmit s = (\x -> x == Submit) <~ s

maybeEvent : (a -> Event) -> Signal (Maybe a) -> Signal Event
maybeEvent f s = (Maybe.withDefault Noop << Maybe.map f) <~ s

blockElement : Int -> Int -> Element -> Element
blockElement w h e = size w h (color gray e)
-- }}}
