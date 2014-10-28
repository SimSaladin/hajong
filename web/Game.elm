module Game where

import Util
import GameTypes (..)

import Graphics.Input (..)
import Maybe (maybe)
import Array

-- the game = { display = display, events = events }

-- Controls ----------------------------------------------------------

-- TODO: ???
type Controls = { hoveredTile : Maybe Tile }

-- Maybe a tile to discard from my hand
discard : Input (Maybe Tile)
discard = input Nothing

discardHover : Input (Maybe Tile)
discardHover = input Nothing

view : Signal Controls
view = Controls <~ dropRepeats discardHover.signal

-- View --------------------------------------------------------------
t_w = 62
t_h = 82

display : Controls -> GameState -> Element
display co gs = case gs.roundState of
   Just rs ->
      let hands      = Array.fromList <| sortBy (\(k,_) -> kazeNth k) rs.hands
          offset     = kazeNth (rs.mypos) - 1
          playerAt n = Array.getOrFail (n + offset % 4)
      in flow down
           [ container 1000 650 midTop <| collage 650 650
               [ dispInfoBlock rs
               , move (70, 70) <| scale 0.6 <| toForm <| dispWanpai co rs
               , scale 0.7 <| dispDiscards co playerAt hands
               ]
           , container 1000 100 midTop <| dispMyHand co rs.myhand
           ] `beside` dispLog rs.actions
   Nothing -> asText "Hmm, roundState is Nothing but I should be in a game"

dispDiscards co playerAt hands = group
   [ moveY (-330) <|                         toForm <| dispHand co <| playerAt 0 hands
   , moveX 330    <| rotate (degrees 90)  <| toForm <| dispHand co <| playerAt 1 hands
   , moveY 330    <| rotate (degrees 180) <| toForm <| dispHand co <| playerAt 2 hands
   , moveX (-330) <| rotate (degrees 270) <| toForm <| dispHand co <| playerAt 3 hands
   ]

dispInfoBlock rs =
   toForm
   <| color black <| container 204 204 middle
   <| color white <| size 200 200
   <| flow down
      [ plainText "Turn of " `beside` dispKaze rs.turn
      , plainText "I am " `beside` dispKaze rs.mypos
      , plainText "Round is " `beside` dispKaze rs.round
      , plainText "dealer is " `beside` dispKaze rs.dealer
      , plainText "tiles left: " `beside` asText rs.tilesleft
      , plainText "players: " `beside` (flow down <| map asText rs.players)
      ]
   -- , asText <| "Results: " ++ show rs.results

dispLog = flow up << map asText

-- Upstream ---------------------------------------------------------------

events : Signal Event
events = merges
   [ maybe Noop (InGameAction << GameTurn << TurnTileDiscard False) <~ discard.signal
   ]

-- My Hand ----------------------------------------------------------------

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

-- Others' hands -----------------------------------------------------------
data Dir = Up | Down | Left | Right
   
rotatedTo dir frm = case dir of
   Up    -> color lightOrange <| collage (6*(t_w+4)+2) (3*(t_h+2)) [rotate (degrees 180) frm]
   Down  -> color lightRed    <| collage (6*(t_w+4)+2) (3*(t_h+4)) [frm]
   Left  -> color lightGreen  <| collage (3*(t_h+4)) (6*(t_w+4)+2) [rotate (degrees 270) frm]
   Right -> color blue        <| collage (3*(t_h+4)) (6*(t_w+4)+2) [rotate (degrees 90) frm]

-- Tiles -----------------------------------------------------------------------
dispTile co tile = container (t_w + 4) (t_h + 4) middle
   <| hoverable discardHover.handle (\h -> if h then Just tile else Nothing)
   <| color (if co.hoveredTile == Just tile then blue else lightBlue)
   <| size t_w t_h
   <| tileImage tile

dispKaze kaze = asText kaze

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


-- Misc -------------------------------------------------------------------------

groupInto n xs = case xs of
   [] -> []
   _  -> take n xs :: groupInto n (drop n xs)

-- State

processInGameEvent : GameEvent -> GameState -> GameState
processInGameEvent event gs = case event of
   RoundPrivateStarts rs ->
        { gs | status <- InGame
             , gameWait <- Nothing
             , roundState <- Just rs }

   RoundPrivateWaitForShout {seconds} ->
      Util.log gs ("Waiting for shouts... " ++ show seconds)

   RoundPrivateChange {hand} -> setMyHand hand gs

   RoundTurnBegins {player} ->
      Util.log gs ("Turn of " ++ show player) |> setTurnPlayer player

   RoundTurnAction {player, action} ->
      addTurnAction player action gs
         |> processTurnAction player action

   RoundTurnShouted {player, shout} ->
      Util.log gs <| show player ++ " shouted: " ++ show shout -- TODO prettify

   RoundHandChanged {player, hand} -> setPlayerHand player hand gs

   RoundEnded res ->
      Util.log gs (show res) -- TODO implement results logic

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
atPlayer k f ((k', h) :: xs) = if k == k' then (k', f h) :: xs
                                          else (k', h)   :: atPlayer k f xs

addDiscard disc h = { h | discards <- h.discards ++ [disc] }
setRiichi riichi h = { h | riichi <- riichi }
updateHand player hand = atPlayer player (\_ -> hand)
