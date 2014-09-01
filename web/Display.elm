module Display where

import Window
import Set
import Connection (..)
import GameTypes (..)
import State (..)

data StateView = Lounge (Viewed AtLounge)
               | Playing (Viewed {})

type AtLounge = { newgame : Element }

type Viewed a = { a | game : GameState }

-- Logs

logView : Viewed a -> Element
logView {game} = above (asText game.debuglog) <| flow down <| map eventView game.eventlog

eventView : Event -> Element
eventView ev = case ev of
    JoinServer {nick}      -> nick ++ " joined server." |> toText |> Text.color green |> leftAligned
    PartServer {nick}      -> nick ++ " has left." |> toText |> Text.color red |> leftAligned
    Message {from,content} -> toText "<" ++ (toText from |> bold) ++ toText "> " ++ toText content |> leftAligned
    Invalid {content}      -> toText content |> Text.color white |> leftAligned |> color red
    LoungeInfo {lounge}    -> toText "Lounge updated - connection established!" |> Text.color blue |> leftAligned
    GameCreated {game}     -> join " " ["Game", game.topic, show game.ident, join " " <| Set.toList game.players] |> toText |> leftAligned
    _ -> asText ev |> color orange

-- Lounge

loungeView : Viewed AtLounge -> Element
loungeView o = flow down
    [ toText "Lounge" |> bold |> centered
    , blockElement 300 400 (gameListView o)
        `beside` spacer 5 5 `beside`
        blockElement 300 400 (gameCreateView o)
    ]

gameCreateView : Viewed AtLounge -> Element
gameCreateView {game, newgame} = flow down
    [ toText "Create a game" |> bold |> centered
    , newgame
    ]

gameListView : Viewed a -> Element
gameListView {game} = flow down
    [ flow down   <| map gameInfoView game.lounge.games
    , leftAligned <| toText "Idle players: " ++ toText (join ", " <| Set.toList game.lounge.idle)
    ]

-- Common

gameInfoView : GameInfo -> Element
gameInfoView {ident,topic,players} = leftAligned <|
        (show ident |> toText |> bold) ++ toText " " ++
        (toText topic)  ++ toText " " ++
        toText (join ", " <| Set.toList players)

blockElement w h e = size w h (color gray e)
