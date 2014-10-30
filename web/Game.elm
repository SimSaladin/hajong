module Game where

import Util
import GameTypes (..)

import Graphics.Input (..)
import Maybe (maybe)
import Array
import Debug

t_w = 62
t_h = 82

-- {{{ Controls ------------------------------------------------------
type Controls = { hoveredTile : Maybe Tile }

controls : Signal Controls
controls = Controls <~ dropRepeats discardHover.signal

-- Maybe a tile to discard from my hand
discard : Input (Maybe Tile)
discard = input Nothing

discardHover : Input (Maybe Tile)
discardHover = input Nothing
-- }}}

-- {{{ Upstream events ----------------------------------------------------
events : Signal Event
events = merges
   [ maybe Noop (InGameAction << GameTurn << TurnTileDiscard False) <~ discard.signal
   ]
-- }}}

-- {{{ Display -------------------------------------------------------
display : Controls -> GameState -> Element
display co gs = case gs.roundState of
   Just rs ->
      let hands      = Array.fromList <| sortBy (\(k,_) -> kazeNth k) rs.hands
          offset     = kazeNth rs.mypos - 1
          playerAt n = Array.getOrFail (n + offset % 4)
      in flow down
           [ container 1000 650 midTop <| collage 650 650
               [ dispInfoBlock playerAt co rs
               , scale 0.7 <| dispDiscards co playerAt hands
               ]
           , container 1000 100 midTop <| dispMyHand co rs.myhand
           , asText (gs.waitTurnAction, gs.waitShout)
           ] `beside` dispLog rs.actions
   Nothing -> asText "Hmm, roundState is Nothing but I should be in a game"
-- }}}

dispDiscards co playerAt hands = group
   [ moveY (-330) <|                         toForm <| dispHand co <| playerAt 0 hands
   , moveX 330    <| rotate (degrees 90)  <| toForm <| dispHand co <| playerAt 1 hands
   , moveY 330    <| rotate (degrees 180) <| toForm <| dispHand co <| playerAt 2 hands
   , moveX (-330) <| rotate (degrees 270) <| toForm <| dispHand co <| playerAt 3 hands
   ]

-- dispInfoBlock : (Int -> Array.Array Int -> Int) -> RoundState -> Form
dispInfoBlock playerAt co rs =
   toForm
   <| color black <| container 234 234 middle
   <| color white <| size 230 230
   <| collage 230 230
      [ toForm <| centered <| bold <| toText <| show rs.round
      , move (-50, 60) <| scale 0.6 <| toForm <| dispWanpai co rs
      , moveY (-30)  <| toForm <| centered <|         toText <| show rs.tilesleft
      , moveY (-100) <|                         toForm <| asText <| playerAt 0 <| Array.fromList rs.players
      , moveX 100    <| rotate (degrees 90)  <| toForm <| asText <| playerAt 1 <| Array.fromList rs.players
      , moveY 100    <| rotate (degrees 180) <| toForm <| asText <| playerAt 2 <| Array.fromList rs.players
      , moveX (-100) <| rotate (degrees 270) <| toForm <| asText <| playerAt 3 <| Array.fromList rs.players
      ]
   -- , asText <| "Results: " ++ show rs.results

dispLog = flow up << map asText

-- {{{ Hands --------------------------------------------------------------
data Dir = Up | Down | Left | Right

dispMyHand co hand = flow right
   [ flow right <| map (dispTileClickable co) <| sortTiles hand.concealed
   , spacer 10 10
   , maybe empty (dispTileClickable co >> color lightGreen) hand.pick
   ]

dispHand co (k, h) =
   container (6*(t_w+4)+2) (3*(t_h+4)) topLeft
   <| flow down
   <| map (flow right)
   <| groupInto 6
   <| map (dispTile co << fst) h.discards
-- }}}
   
-- {{{ Utility
rotatedTo dir frm = case dir of
   Up    -> color lightOrange <| collage (6*(t_w+4)+2) (3*(t_h+2)) [rotate (degrees 180) frm]
   Down  -> color lightRed    <| collage (6*(t_w+4)+2) (3*(t_h+4)) [frm]
   Left  -> color lightGreen  <| collage (3*(t_h+4)) (6*(t_w+4)+2) [rotate (degrees 270) frm]
   Right -> color blue        <| collage (3*(t_h+4)) (6*(t_w+4)+2) [rotate (degrees 90) frm]
-- }}}

-- {{{ Tiles -----------------------------------------------------------------------
dispTile : Controls -> Tile -> Element
dispTile co tile = container (t_w + 4) (t_h + 4) middle
   <| hoverable discardHover.handle (\h -> if h then Just tile else Nothing)
   <| color (if co.hoveredTile == Just tile then blue else lightBlue)
   <| size t_w t_h
   <| tileImage tile

dispWanpai : Controls -> RoundState -> Element
dispWanpai co = .dora >> map (dispTile co) >> flow right

dispTileClickable : Controls -> Tile -> Element
dispTileClickable co tile = dispTile co tile |> clickable discard.handle (Just tile)

tileImage tile =
   let (row, col) = case tile of
               Suited ManTile n _  -> (125 + (n - 1) * 97, 47)
               Suited PinTile n _  -> (125 + (n - 1) * 97, 142)
               Suited SouTile n _  -> (125 + (n - 1) * 97, 237)
               Honor (Kazehai k)   -> (222 + (kazeNth k - 1) * 97, 356)
               Honor (Sangenpai s) -> (707 + (sangenNth s - 1) * 97, 356)
   in
      croppedImage (row, col) 62 82 "Mahjong-tiles.jpg"
-- }}}

-- {{{ Misc -------------------------------------------------------------------------
groupInto n xs = case xs of
   [] -> []
   _  -> take n xs :: groupInto n (drop n xs)
-- }}}

-- {{{ Process GameEvents
processInGameEvent : GameEvent -> GameState -> GameState
processInGameEvent event gs = case event of
   RoundPrivateStarts rs ->
        { gs | status <- InGame
             , gameWait <- Nothing
             , roundState <- Just rs }

   RoundPrivateWaitForTurnAction {seconds} -> { gs | waitTurnAction <- Just seconds }

   RoundPrivateWaitForShout {seconds} -> { gs | waitShout <- Just seconds }

   RoundPrivateChange {hand} -> setMyHand hand gs

   RoundTurnBegins {player_kaze} ->
      Util.log ("Turn of " ++ show player_kaze) gs |> setTurnPlayer player_kaze

   RoundTurnAction {player_kaze, action} ->
      addTurnAction player_kaze action gs
         |> processTurnAction player_kaze action

   RoundTurnShouted {player_kaze, shout} ->
      Util.log (show player_kaze ++ " shouted: " ++ show shout) gs

   RoundHandChanged {player_kaze, hand} -> setPlayerHand player_kaze hand gs

   RoundEnded res ->
      Util.log (show res) gs -- TODO implement results logic

-- Field modify boilerplate --------------------------------------------------
setMyHand hand gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | myhand <- hand } }

setTurnPlayer player gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | turn <- player } }

addTurnAction : Kaze -> TurnAction -> GameState -> GameState
addTurnAction player action gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | actions <- (player, action) :: rs.actions } }

setPlayerHand player hand gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | hands <- updateHand player hand rs.hands } }

-- Turn logic -----------------------------------------------------------------
processTurnAction : Kaze -> TurnAction -> GameState -> GameState
processTurnAction player action gs =
   case gs.roundState of
      Just rs -> case action of
         TurnTileDiscard riichi tile ->
            { gs | roundState <- Just { rs | hands <- atPlayer player
                  (\h -> { h | discards <- h.discards ++ [(tile, Nothing)]
                             , riichi   <- riichi }) rs.hands
            }}
         TurnTileDraw _ _ ->
            { gs | roundState <- Just { rs | tilesleft <- rs.tilesleft - 1 }
            }
         TurnAnkan tile  ->
            { gs | roundState <- Just { rs | hands <- atPlayer player
               (\h -> { h | called <- h.called ++ [ kantsu tile ] }) rs.hands }
            }

kantsu : Tile -> Mentsu
kantsu t = Mentsu Kantsu t Nothing

atPlayer : Kaze -> (a -> a) -> [(Kaze, a)] -> [(Kaze, a)]
atPlayer k f xs = case xs of
   ((k', h) :: xs) -> if k == k' then (k', f h) :: xs
                                 else (k', h)   :: atPlayer k f xs
   [] -> Debug.crash <| "Player "++ show k ++ " not found in " ++ show xs

addDiscard disc h = { h | discards <- h.discards ++ [disc] }
setRiichi riichi h = { h | riichi <- riichi }
updateHand player hand = atPlayer player (\_ -> hand)
-- }}}
