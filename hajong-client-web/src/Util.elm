module Util where

import GameTypes exposing (..)
import PlayerInfo
import Maybe

import Text
import Signal
import Dict
import Dict exposing (Dict)
import Debug
import Graphics.Element as Element
import Color exposing (..)

lookupGameInfo : { a | lounge : LoungeData } -> Int -> Maybe GameInfo
lookupGameInfo game ident = List.head <| List.filter (\g -> g.ident == ident) game.lounge.games

log : String -> { a | logging : List LogItem } -> { a | logging : List LogItem }
log str gs = { gs | logging = LogDebug {msg = str } :: gs.logging }

groupInto : Int -> Int -> List a -> List (List a)
groupInto limit n xs = case xs of
   [] -> []
   _  -> if limit == 0 then [ xs ] else List.take n xs :: groupInto (limit - 1) n (List.drop n xs)

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

listFindWith : (a -> k) -> k -> List a -> Maybe a
listFindWith f k xs = case xs of
   y :: ys -> if f y == k then Just y else listFindWith f k ys
   []      -> Nothing

fromJust : Maybe a -> a
fromJust x = case x of
   Nothing -> Debug.crash "fromJust: Nothing"
   Just y -> y

maybe : a -> (b -> a) -> Maybe b -> a
maybe def f m = Maybe.withDefault def <| Maybe.map f m

isNothing : Maybe a -> Bool
isNothing x = case x of
   Nothing -> True
   Just _ -> False

pickedTile : PickedTile -> Tile
pickedTile pt = case pt of
   FromWall t         -> fromJust t
   FromWanpai t       -> fromJust t
   AgariTsumo t       -> t
   AgariCall shout    -> shout.shoutTile
   AgariTsumoWanpai t -> t

renderTitle title = Text.fromString title |> Text.bold |> Element.centered

-- | An event signal, where Nothing is a Noop
maybeEvent : (a -> Event) -> Signal (Maybe a) -> Signal Event
maybeEvent f s = Signal.map (Maybe.withDefault Noop << Maybe.map f) s

lookupResource : {a | resources : Dict String String } -> String -> String
lookupResource st k = case Dict.get k st.resources of
   Just x -> x
   Nothing -> "resource-not-found.jpg"

-- getInfoWithDefault st "nick" .displayName nick
-- getInfoWithDefault st "http://url/somedefault.jpg" .profilepicture nick
getInfoWithDefault : GameState -> a -> (PlayerInfo.PlayerInfo -> a) -> String -> a
getInfoWithDefault st def f k = Dict.get k st.playerInfo
    |> Maybe.map f |> Maybe.withDefault def
