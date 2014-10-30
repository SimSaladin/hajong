module Util where

import GameTypes (..)

import Debug

atLounge : GameState -> Bool
atLounge = (\s -> s.status == InLounge)

inGame : GameState -> Bool
inGame = (\s -> s.status == InGame)

lookupGameInfo : GameState -> Int -> GameInfo
lookupGameInfo game ident = head <| filter (\g -> g.ident == ident) game.lounge.games

log : String -> GameState -> GameState
log str gs = { gs | debuglog <- str :: gs.debuglog }

groupInto n xs = case xs of
   [] -> []
   _  -> take n xs :: groupInto n (drop n xs)

listModify : k -> (a -> a) -> [(k, a)] -> [(k, a)]
listModify k f xs = case xs of
   (k', h) :: xs -> if k == k' then (k', f h) :: xs
                               else (k', h)   :: listModify k f xs
   [] -> Debug.crash <| "Key " ++ show k ++ " not found in " ++ show xs

listFind : k -> [(k, a)] -> a
listFind k xs = case xs of
   (k', a) :: xs -> if k == k' then a
                               else listFind k xs
   [] -> Debug.crash <| "Key " ++ show k ++ " not found in " ++ show xs
