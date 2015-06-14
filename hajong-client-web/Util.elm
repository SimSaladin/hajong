module Util where

import GameTypes exposing (..)
import Maybe

import Debug

atLounge : GameState -> Bool
atLounge = (\s -> s.status == InLounge)

inGame : GameState -> Bool
inGame = (\s -> s.status == InGame)

lookupGameInfo : GameState -> Int -> Maybe GameInfo
lookupGameInfo game ident = List.head <| List.filter (\g -> g.ident == ident) game.lounge.games

log : String -> GameState -> GameState
log str gs = { gs | debuglog <- str :: gs.debuglog }

groupInto n xs = case xs of
   [] -> []
   _  -> List.take n xs :: groupInto n (List.drop n xs)

listModify : k -> (a -> a) -> List (k, a) -> List (k, a)
listModify k f xs = case xs of
   (k', h) :: xs -> if k == k' then (k', f h) :: xs
                               else (k', h)   :: listModify k f xs
   [] -> Debug.crash <| "Key " ++ toString k ++ " not found in " ++ toString xs

listFind : k -> List (k, a) -> a
listFind k xs = case xs of
   (k', a) :: xs -> if k == k' then a
                               else listFind k xs
   [] -> Debug.crash <| "Key " ++ toString k ++ " not found in " ++ toString xs

fromJust : Maybe a -> a
fromJust (Just x) = x

maybe : a -> (b -> a) -> Maybe b -> a
maybe def f m = Maybe.withDefault def <| Maybe.map f m

isNothing : Maybe a -> Bool
isNothing x = case x of
   Nothing -> True
   Just _ -> False

-- TODO this is not complete
pickedTile : PickedTile -> Tile
pickedTile pt = case pt of
   FromWall (Just t) -> t
   FromWanpai (Just t) -> t
   AgariTsumo t -> t
   AgariCall t _ -> t
   AgariChankan t _ -> t

