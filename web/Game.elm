module Game where

import Util
import GameTypes (..)

import Graphics.Input (..)
import Maybe (maybe, isNothing)
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

shout : Input (Maybe Shout)
shout = input Nothing

ankan : Input (Maybe Tile)
ankan = input Nothing

nocare : Input Bool
nocare = input False

shoutChooseTile : Input (Maybe Tile)
shoutChooseTile = input Nothing
-- }}}

-- {{{ Upstream events ----------------------------------------------------
events : Signal Event
events = merges
   [ maybe Noop (InGameAction << GameTurn << TurnTileDiscard False) <~ discard.signal
   , maybe Noop (InGameAction << GameShout) <~ shout.signal
   , maybe Noop (InGameAction << GameTurn << TurnAnkan) <~ ankan.signal
   , (\x -> if x then InGameAction GameDontCare else Noop) <~ nocare.signal
   ]

getShout : Maybe ShoutKind -> Maybe Shout
getShout = maybe Nothing (\k -> Just <| Shout k Ton (Suited ManTile 1 False) [])
-- }}}

-- {{{ Display -------------------------------------------------------
display : Controls -> GameState -> Element
display co gs = case gs.roundState of
   Just rs -> flow down
        [ container 1000 650 midTop <| collage 650 650
            [ dispInfoBlock co gs rs
            , dispDiscards co gs rs |> scale 0.7
            , dispCalled co gs rs |> scale 0.8
            ]
        , container 1000 40 midTop <| flow right
           <| (
              maybe [] (map shoutButton << snd) (gs.waitShout)
               ++ [ankanButton rs "Ankan", nocareButton gs.waitShout "Pass" ]
               )
        , container 1000 100 midTop <| dispHand rs.mypos co rs.myhand
        ]
   Nothing -> asText "Hmm, roundState is Nothing but I should be in a game"
-- }}}

-- {{{ Per-player ---------------------------------------------------
dispDiscards : Controls -> GameState -> RoundState -> Form
dispDiscards co gs rs = group <| map
   (\k -> Util.listFind k rs.hands
       |> dispHandDiscards co
       |> toForm
       |> moveRotateKaze 330 rs.mypos k)
   [Ton, Nan, Shaa, Pei]

dispCalled co gs rs = group <| map
   (\k -> Util.listFind k rs.hands
      |> dispPublicMentsu co k
      |> toForm
      |> moveRotateKaze 500 rs.mypos k)
   ([Ton, Nan, Shaa, Pei] |> filter (\x -> x /= rs.mypos))

moveRotateKaze : Float -> Kaze -> Kaze -> Form -> Form
moveRotateKaze off mypos pos =
   case (kazeNth pos - kazeNth mypos) % 4 of
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
turnIndicator gs =
   let col = if isNothing gs.waitShout then lightGreen else lightOrange
                in group
   [ rotate (degrees 90) <| filled col <| ngon 3 80
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
dispHand k co hand = flow right
   [ flow right <| map (dispTileClickable co) <| sortTiles hand.concealed
   , spacer 10 10
   , maybe empty (dispTileClickable co >> color lightGreen) hand.pick
   , spacer 10 10
   , flow right <| map (dispMentsu co k) hand.called
   ]

dispHandDiscards co h =
   container (6*(t_w+4)+2) (3*(t_h+4)) topLeft
   <| flow down
   <| map (flow right)
   <| Util.groupInto 6
   <| map (dispTile co << fst) h.discards

dispPublicMentsu co k h = flow right <| map (dispMentsu co k) h.called

dispMentsu : Controls -> Kaze -> Mentsu -> Element
dispMentsu co k m = case m.from of
   Nothing -> empty -- We display only shouted
   Just s  -> flow right <|
    (collage t_h t_w [rotate (degrees 90) <| toForm <| dispTile co s.shoutTile])
    :: map (dispTile co) s.shoutTo
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
               Honor (Kazehai k)   -> (222 + (kazeNth k) * 97, 356)
               Honor (Sangenpai s) -> (707 + (sangenNth s) * 97, 356)
   in
      croppedImage (row, col) 62 82 "Mahjong-tiles.jpg"
-- }}}

-- {{{ Buttons 'n stuff --------------------------------------------------------

shoutButton : Shout -> Element
shoutButton s =
   clickable shout.handle (Just s) <| collage 80 40
      [ oval 80 40 |> filled green
      , toText (show s.shoutKind) |> centered |> toForm ]
      -- TODO multiple chii identify

ankanButton rs str = case findFourTiles rs of
   Just t  -> clickable ankan.handle (Just t) (buttonElem' str green)
   Nothing -> empty

nocareButton w str = case w of
   Just (w, _ :: _) -> clickable nocare.handle True <| buttonElem' str green
   _                -> empty

findFourTiles : RoundState -> Maybe Tile
findFourTiles rs = counted 4 <| rs.myhand.concealed

counted : Int -> [Tile] -> Maybe Tile
counted m ts =
   let go n xs = if n == 0
           then Just (head xs)
           else case xs of
               x :: y :: zs -> if x == y then go (n - 1) (y :: zs)
                                         else counted m (y :: zs)
               _ -> Nothing
   in go m ts

buttonElem' str col = collage 80 40
   [ oval 80 40 |> filled col
   , toText str |> centered |> toForm ]
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
   RoundPrivateWaitForShout {seconds, shouts} ->
      { gs | waitShout      <- Just <| (WaitRecord seconds gs.updated, shouts) }

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
            { gs | roundState <-
                  Just { rs | hands  <- Util.listModify player
                                          (\h -> { h | discards <- h.discards ++ [(tile, Nothing)]
                                                     , riichi <- riichi }) rs.hands
                       }
                 , waitShout <- Just (WaitRecord 10 gs.updated, [])
            }
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
-- }}}
