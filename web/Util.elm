module Util where

import GameTypes (..)

atLounge : GameState -> Bool
atLounge = (\s -> s.status == InLounge)

inGame : GameState -> Bool
inGame = (\s -> s.status == InGame)

lookupGameInfo : GameState -> Int -> GameInfo
lookupGameInfo game ident = head <| filter (\g -> g.ident == ident) game.lounge.games

log : String -> GameState -> GameState
log str gs = { gs | debuglog <- str :: gs.debuglog }
