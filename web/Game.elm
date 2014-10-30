module Game where

import Util
import GameTypes (..)

import Graphics.Input (..)
import Maybe (maybe)
import Array

-- TODO: hard-coded tile widths; some other measures are also hard-coded
t_w = 62
t_h = 82

-- {{{ Controls ------------------------------------------------------
type Controls = { hoveredTile : Maybe Tile
                }

controls : Signal Controls
controls = Controls <~ dropRepeats discardHover.signal

-- Maybe a tile to discard from my hand
discard : Input (Maybe Tile)
discard = input Nothing

discardHover : Input (Maybe Tile)
discardHover = input Nothing

shout : Input (Maybe ShoutKind)
shout = input Nothing

shoutChooseTile : Input (Maybe Tile)
shoutChooseTile = input Nothing
-- }}}

-- {{{ Upstream events ----------------------------------------------------
events : Signal Event
events = merges
   [ maybe Noop (InGameAction << GameTurn << TurnTileDiscard False) <~ discard.signal
   --, maybe Noop (InGameAction << GameTurn << GameShout) <~ shoutEvent
   ]

-- TODO: implement me
-- shoutEvent : Signal (Maybe Shout)
-- shoutEvent = Shout <~ shout.signal ~ gameState

-- }}}

-- {{{ Display -------------------------------------------------------
display : Controls -> GameState -> Element
display co gs = case gs.roundState of
   Just rs -> flow down
        [ container 1000 650 midTop <| collage 650 650
            [ dispInfoBlock co gs rs
            , dispDiscards co gs rs |> scale 0.7
            ]
        , container 1000 40 midTop <| flow right
           [ shoutButton Kan "Kan"
           , shoutButton Pon "Pon"
           , shoutButton Chi "Chi"
           , shoutButton Ron "Ron"
           ]
        , container 1000 100 midTop <| dispHand co rs.myhand
        , asText gs.waitShout
        ] `beside` dispLog rs.actions
   Nothing -> asText "Hmm, roundState is Nothing but I should be in a game"

dispLog = flow up << map asText
-- }}}

-- {{{ Per-player ---------------------------------------------------
dispDiscards : Controls -> GameState -> RoundState -> Form
dispDiscards co gs rs = group <| map
   (\k -> Util.listFind k rs.hands
       |> dispPublicHand co
       |> toForm
       |> moveRotateKaze 330 rs.mypos k)
   [Ton, Nan, Shaa, Pei]

moveRotateKaze : Float -> Kaze -> Kaze -> Form -> Form
moveRotateKaze off mypos pos =
   case (kazeNth pos + kazeNth mypos - 2) % 4 of
      0 -> moveY (-off)
      1 -> moveX off    << rotate (degrees 90) 
      2 -> moveY off    << rotate (degrees 180)
      3 -> moveX (-off) << rotate (degrees 270)
-- }}}

-- {{{ Info block ----------------------------------------------------
dispInfoBlock co gs rs =
   toForm
   <| color black <| container 234 234 middle
   <| color white <| size 230 230
   <| collage 230 230 (
      [ turnIndicator gs |> moveRotateKaze 90 rs.mypos rs.turn
      , toForm <| centered <| bold <| toText <| show rs.round
      , move (-60, 60) <| scale 0.6 <| toForm <| dispWanpai co rs
      , moveY (-30) <| toForm <| centered <| toText <| show rs.tilesleft
      ]
      ++ map (\k -> dispPlayerInfo rs k |> moveRotateKaze 100 rs.mypos k)
             [Ton, Nan, Shaa, Pei]
      )

turnIndicator : GameState -> Form
turnIndicator gs = group
   [ rotate (degrees 90) <| filled lightGreen <| ngon 3 80
   , moveY 30
         <| toForm <| asText <| floor
         <| case gs.waitTurnAction of
               Nothing -> inSeconds <| gs.updated - gs.turnBegan
               Just wr -> toFloat wr.seconds - (inSeconds (gs.updated - wr.added))
   ]

dispPlayerInfo rs k = flow right
   [ asText (Util.listFind k rs.players)
   , spacer 5 5
   , asText k
   , spacer 5 5
   , asText (Util.listFind k rs.points)
   ] |> toForm
-- }}}

-- {{{ Hands --------------------------------------------------------------
dispHand co hand = flow right
   [ flow right <| map (dispTileClickable co) <| sortTiles hand.concealed
   , spacer 10 10
   , maybe empty (dispTileClickable co >> color lightGreen) hand.pick
   ]

dispPublicHand co h =
   container (6*(t_w+4)+2) (3*(t_h+4)) topLeft
   <| flow down
   <| map (flow right)
   <| Util.groupInto 6
   <| map (dispTile co << fst) h.discards
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

-- {{{ Buttons 'n stuff --------------------------------------------------------
shoutButton s = button shout.handle (Just s)
-- }}}

-- {{{ Process GameEvents
processInGameEvent : GameEvent -> GameState -> GameState
processInGameEvent event gs = case event of
   RoundPrivateStarts rs ->
        { gs | status <- InGame
             , gameWait <- Nothing
             , roundState <- Just rs }

   RoundPrivateWaitForTurnAction {seconds} ->
      { gs | waitTurnAction <- Just <| WaitRecord seconds gs.updated }
   RoundPrivateWaitForShout {seconds} ->
      { gs | waitShout      <- Just <| WaitRecord seconds gs.updated }

   RoundPrivateChange {hand} -> setMyHand hand gs

   RoundTurnBegins {player_kaze} ->
      Util.log ("Turn of " ++ show player_kaze) gs
      |> setTurnPlayer player_kaze
      |> \gs -> { gs | turnBegan <- gs.updated
                     , waitTurnAction <- Nothing
                     , waitShout <- Nothing
                }

   RoundTurnAction {player_kaze, action} ->
      addTurnAction player_kaze action gs
         |> processTurnAction player_kaze action

   RoundTurnShouted {player_kaze, shout} ->
      Util.log (show player_kaze ++ " shouted: " ++ show shout) gs

   RoundHandChanged {player_kaze, hand} -> setPlayerHand player_kaze hand gs

   RoundEnded res ->
      Util.log (show res) gs -- TODO implement results logic
-- }}}

-- {{{ Field modify boilerplate ----------------------------------------------
setMyHand hand gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | myhand <- hand } }

setTurnPlayer player gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | turn <- player } }

addTurnAction : Kaze -> TurnAction -> GameState -> GameState
addTurnAction player action gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | actions <- (player, action) :: rs.actions } }

setPlayerHand player hand gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | hands <- updateHand player hand rs.hands } }
-- }}}

-- {{{ Turns ------------------------------------------------------------------
processTurnAction : Kaze -> TurnAction -> GameState -> GameState
processTurnAction player action gs =
   case gs.roundState of
      Just rs -> case action of
         TurnTileDiscard riichi tile ->
            { gs | roundState <- Just { rs | hands <- Util.listModify player
                  (\h -> { h | discards <- h.discards ++ [(tile, Nothing)]
                             , riichi   <- riichi }) rs.hands
            }}
         TurnTileDraw _ _ ->
            { gs | roundState <- Just { rs | tilesleft <- rs.tilesleft - 1 }
            }
         TurnAnkan tile  ->
            { gs | roundState <- Just { rs | hands <- Util.listModify player
               (\h -> { h | called <- h.called ++ [ kantsu tile ] }) rs.hands }
            }

kantsu : Tile -> Mentsu
kantsu t = Mentsu Kantsu t Nothing

addDiscard disc h = { h | discards <- h.discards ++ [disc] }
setRiichi riichi h = { h | riichi <- riichi }
updateHand player hand = Util.listModify player (\_ -> hand)
-- }}}
