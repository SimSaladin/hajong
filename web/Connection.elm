module Connection where

import Game (processInGameEvent)
import GameTypes (..)
import JSON (fromJSON_Event, toJSON_Event)

import Json
import WebSocket

-- WebSocket connect --------------------------------------------------------
connect : Signal Event -> Signal Event
connect output = dropRepeats
    <| fromJSON_Event
    <~ WebSocket.connect "ws://localhost:9160" (Json.toString "" << toJSON_Event <~ output)
                -- TODO dropRepeats should not be necessary here

