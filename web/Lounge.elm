module Lounge where

import GameTypes (..)
import State (..)
import Connection as Conn

import Maybe (maybe)
import Set
import Text
import Graphics.Input (..)
import Graphics.Input.Field as Field

data ButtonState = Submit | Clear

-- Helpers

isSubmit : Signal ButtonState -> Signal Bool
isSubmit s = (\x -> x == Submit) <~ s

maybeEvent : (a -> Conn.Event) -> Signal (Maybe a) -> Signal Conn.Event
maybeEvent f s = maybe Conn.Noop f <~ s

-- Upstream events --------------------------------

events : Signal Conn.Event
events = merges
    [ Conn.createGame <~ newGameCreated
    , maybeEvent (Conn.joinGame << .ident) joined
    , forceStartEvent
    ]

-- views ------------------------------------------

type View = { chosenGame  : Maybe GameInfo
            , newGameForm : Element
            }

view : Signal View
view = View <~ chosenGame.signal
              ~ newGameForm

display : View -> GameState -> Element
display v gs = doDraw { v | game = gs }

doDraw o = flow down
    [ maybe (spacer 10 10 |> color red) (waitView << lookupGameInfo o.game) o.game.gameWait
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
    , o.newGameForm
    , spacer 5 5
    ]

gameListView o = flow down
    [ toText "Available games" |> bold |> centered
    , flow down   <| buildGameList o.chosenGame o.game.lounge.games
    , joinGameButton
    , leftAligned <| toText "Idle players: " ++ toText (join ", " <| Set.toList o.game.lounge.idle)
    ]

blockElement : Int -> Int -> Element -> Element
blockElement w h e = size w h (color gray e)

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

-- UI --------------------------------------------------

-- Force start
forceStart         = input Nothing
forceStartEvent    = maybeEvent Conn.forceStart forceStart.signal
forceStartButton n = button forceStart.handle (Just n) "Force start"

-- New game
createGame = input Clear
topicField = input Field.noContent
newGameCreated = .string <~ sampleOn (isSubmit createGame.signal) topicField.signal
topicfs = topicField.signal
newGameForm    = flow down <~ combine
   [ Field.field Field.defaultStyle topicField.handle identity "Topic" <~ topicField.signal
   , constant <| button createGame.handle Submit "Create"
   ]

-- Join game
chosenGame     = input Nothing
joinGame       = input Clear
joinGameButton = button joinGame.handle Submit "Join"
joined         = sampleOn (isSubmit joinGame.signal) chosenGame.signal

gameListElement : GameInfo -> Element -> Element
gameListElement game = clickable chosenGame.handle (Just game)
