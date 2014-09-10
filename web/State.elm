module State where

import Connection (..)
import GameTypes (..)

-- Game ----------------------------------------------------------------------

data Status = InLounge | InGame

type GameState = { status   : Status
                 , mynick   : String
                 , lounge   : LoungeData
                 , gameWait : Maybe Int
                 , mousepos : (Int, Int)
                 , eventlog : [Event]
                 , debuglog : String
                 }

defaultGame : GameState
defaultGame = { status = InLounge
              , mynick = "mynick" -- TODO
              , lounge = defaultLounge
              , gameWait = Nothing
              , mousepos = (0,0)
              , eventlog = []
              , debuglog = ""
              }

atLounge : GameState -> Bool
atLounge = (\s -> s.status == InLounge)

inGame : GameState -> Bool
inGame = (\s -> s.status == InGame)

lookupGameInfo : GameState -> Int -> GameInfo
lookupGameInfo game ident = head <| filter (\g -> g.ident == ident) game.lounge.games
