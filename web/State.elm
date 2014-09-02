module State where

import Connection (..)
import GameTypes (..)

-- Game ----------------------------------------------------------------------

data Status = InLounge | InGame

type GameState = { status   : Status
                 , mynick   : String
                 , lounge   : LoungeData
                 , gameSel  : Maybe GameInfo
                 , gameWait : Maybe Int
                 , mousepos : (Int, Int)
                 , eventlog : [Event]
                 , debuglog : String
                 }

defaultGame : GameState
defaultGame = { status = InLounge
              , mynick = "mynick" -- TODO
              , lounge = defaultLounge
              , gameSel = Nothing
              , gameWait = Nothing
              , mousepos = (0,0)
              , eventlog = []
              , debuglog = ""
              }

atLounge : GameState -> Bool
atLounge = (\s -> s.status == InLounge)

lookupGameInfo : GameState -> Int -> GameInfo
lookupGameInfo game ident = head <| filter (\g -> g.ident == ident) game.lounge.games
