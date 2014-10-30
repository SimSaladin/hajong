module Lounge where

import GameTypes (..)
import Events
import Util

import Maybe (maybe)
import Set
import Text
import Keyboard
import Graphics.Input (..)
import Graphics.Input.Field as Field

data ButtonState = Submit | Clear

-- {{{ Controls ----------------------------------------
type Controls = { chosen : Maybe GameInfo
                , topic  : Field.Content
                }

controls : Signal Controls
controls = Controls <~ chosenGame.signal
                     ~ topic.signal
-- }}}

-- {{{ Events ------------------------------------------
events : Signal Event
events = merges
    [ Events.createGame <~ newGameEvent
    , maybeEvent (Events.joinGame << .ident) joined
    , forceStartEvent
    ]
-- }}}

-- {{{ Display -----------------------------------------
display : Controls -> GameState -> Element
display v gs = doDraw { v | game = gs }

doDraw o = flow down
    [ maybe (spacer 10 10 |> color red) (waitView << Util.lookupGameInfo o.game) o.game.gameWait
    , toText "Lounge" |> bold |> centered
    , blockElement 250 400 (gameListView o)
        `beside` spacer 5 5 `beside`
        blockElement 250 400 (gameCreateView o)
    , spacer 5 5
    ]

waitView gameinfo = color lightOrange <| flow right
    [ toText "Waiting for " |> leftAligned
    , gameInfoView gameinfo
    , forceStartButton gameinfo.ident
    ]

gameCreateView o = flow down
    [ toText "Create a game" |> bold |> centered
    , topicField o.topic
    , createButton
    , spacer 5 5
    ]

gameListView o = flow down
    [ toText "Available games" |> bold |> centered
    , flow down   <| buildGameList o.chosen o.game.lounge.games
    , joinGameButton
    , leftAligned <| toText "Idle players: " ++ toText (join ", " <| Set.toList o.game.lounge.idle)
    ]
-- }}}

-- {{{ Game info --------------------------------------

-- | `active_game all_games`
buildGameList : Maybe GameInfo -> [GameInfo] -> [Element]
buildGameList act =
    let active gi = if act == Just gi then color blue else identity
    in  map (\gi -> active gi <| gameListElement gi <| gameInfoView gi)

gameInfoView : GameInfo -> Element
gameInfoView {ident,topic,players} =
    let pc = length <| Set.toList players
    in leftAligned <|
        (show ident |> toText |> Text.color red) ++
        toText " " ++ (toText topic |> bold)  ++ toText " " ++
        toText " (" ++ (show pc |> toText |> bold) ++ toText "/4 players)" ++
        toText " {" ++ toText (join ", " <| Set.toList players) ++ toText "}"

gameListElement : GameInfo -> Element -> Element
gameListElement game = clickable chosenGame.handle (Just game)
-- }}}

-- {{{ Force start of the game -------------------------
forceStart : Input (Maybe Int)
forceStart = input Nothing

forceStartEvent    = maybeEvent Events.forceStart forceStart.signal
forceStartButton n = button forceStart.handle (Just n) "Force start"
-- }}}

-- {{{ New Game (settings) -----------------------------
topic  = input Field.noContent -- TODO: generalize to settings
create = input Clear

-- | New game settings; sampled when create-game button
newGameEvent = .string <~ sampleOn (isSubmit create.signal) topic.signal

createButton = button create.handle Submit "Create"
topicField   = Field.field Field.defaultStyle topic.handle identity "Topic"

-- }}}

-- {{{ Join game ---------------------------------------
chosenGame     = input Nothing
joinGame       = input Clear

joinGameButton = button joinGame.handle Submit "Join"
joined         = sampleOn (isSubmit joinGame.signal) chosenGame.signal
-- }}}

-- {{{ Helpers -----------------------------------------
isSubmit : Signal ButtonState -> Signal Bool
isSubmit s = (\x -> x == Submit) <~ s

maybeEvent : (a -> Event) -> Signal (Maybe a) -> Signal Event
maybeEvent f s = maybe Noop f <~ s

blockElement : Int -> Int -> Element -> Element
blockElement w h e = size w h (color gray e)
-- }}}
