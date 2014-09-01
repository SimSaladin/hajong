module State where

import Connection (..)
import GameTypes (..)

-- Game ----------------------------------------------------------------------

data Status = InLounge | InGame

type GameState = { status   : Status
                 , lounge   : LoungeData
                 , mousepos : (Int, Int)
                 , eventlog : [Event]
                 , debuglog : String
                 }

defaultGame : GameState
defaultGame = { status = InLounge
              , lounge = defaultLounge
              , mousepos = (0,0)
              , eventlog = []
              , debuglog = ""
              }
