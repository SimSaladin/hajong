module Display where

import Window
import Html
import Set

import Connection (..)
import GameTypes (..)
import State (..)

type AtLounge = { doJoin   : Element
                , gamelist : [Element]
                , newgame  : Element
                }

type Viewed a = { a | game : GameState }

-- Log

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
    _                      -> asText ev |> color orange

-- Lounge

loungeView : Viewed AtLounge -> Element
loungeView o = flow down
    [ maybe (spacer 10 10 |> color red) waitView o.game.gameWait
    , toText "Lounge" |> bold |> centered
    , blockElement 300 400 (gameListView o)
        `beside` spacer 5 5 `beside`
        blockElement 300 400 (gameCreateView o)
    , spacer 5 5
    ]

waitView game = flow right
    [ toText "Waiting for " |> leftAligned
    , (gameInfoView game |> color purple)
    ]

gameCreateView {game, newgame} = flow down
    [ toText "Create a game" |> bold |> centered
    , newgame
    ]

gameListView {game, gamelist, doJoin} = flow down
    [ flow down   <| gamelist
    , doJoin
    , leftAligned <| toText "Idle players: " ++ toText (join ", " <| Set.toList game.lounge.idle)
    ]

-- Common

gameInfoView : GameInfo -> Element
gameInfoView {ident,topic,players} =
    let pc = length <| Set.toList players
    in leftAligned <|
        (show ident |> toText |> Text.color red) ++
        toText " " ++ (toText topic |> bold)  ++ toText " " ++
        toText " (" ++ (show pc |> toText |> bold) ++ toText "/4 players)" ++
        toText " {" ++ toText (join ", " <| Set.toList players) ++ toText "}"

blockElement : Int -> Int -> Element -> Element
blockElement w h e = size w h (color gray e)
