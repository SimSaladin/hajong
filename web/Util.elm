module Util where

import GameTypes (..)

atLounge : GameState -> Bool
atLounge = (\s -> s.status == InLounge)

inGame : GameState -> Bool
inGame = (\s -> s.status == InGame)

lookupGameInfo : GameState -> Int -> GameInfo
lookupGameInfo game ident = head <| filter (\g -> g.ident == ident) game.lounge.games

log : GameState -> String -> GameState
log gs str = { gs | debuglog <- str :: gs.debuglog }
