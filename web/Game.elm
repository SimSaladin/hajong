module Game where

import Util
import GameTypes (..)

import Graphics.Input (..)
import Maybe (maybe, isNothing)
import List
import Array

-- TODO: hard-coded tile widths; some other measures are also hard-coded
t_w = 62
t_h = 82
discards_off = 330
called_off   = 650

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

shouminkan : Input (Maybe Tile)
shouminkan = input Nothing

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
   , maybe Noop (InGameAction << GameTurn << TurnShouminkan) <~ shouminkan.signal
   , (\x -> if x then InGameAction GameDontCare else Noop) <~ nocare.signal
   ]

getShout : Maybe ShoutKind -> Maybe Shout
getShout = maybe Nothing (\k -> Just <| Shout k Ton (Suited ManTile 1 False) [])
-- }}}

-- {{{ Display -------------------------------------------------------
display : Controls -> GameState -> Element
display co gs = case gs.roundState of
   Just rs -> flow down
        [ container 1000 650 midTop <| collage 1000 650
            [ dispInfoBlock co gs rs
            , dispDiscards co gs rs |> scale 0.7
            , dispCalled co gs rs |> scale 0.6
            , maybe (toForm empty) dispResults rs.results
            ]
        , container 1000 40 midTop <| flow right
           <| (
              maybe [] (map shoutButton << snd) (gs.waitShout)
               ++ shouminkanButtons rs "Shouminkan"
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
       |> moveRotateKaze discards_off rs.mypos k)
   [Ton, Nan, Shaa, Pei]

dispCalled co gs rs = [Ton, Nan, Shaa, Pei]
   |> filter (\x -> x /= rs.mypos)
   |> map (
      \k -> Util.listFind k rs.hands
         |> dispPublicMentsu co k
         |> toForm
         |> moveRotateKaze called_off rs.mypos k
      )
   |> group

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

dispPlayerInfo rs k =
   let (p, points, name) = Util.listFind k rs.players
   in asText p `beside` ((if name == "" then toText "(bot)" else toText name |> bold) |> centered)
      `above` flow right
      [ spacer 5 5, asText k, spacer 5 5
      , asText points
      ] |> toForm
-- }}}

-- {{{ Results
dispResults : RoundResult -> Form
dispResults res =
   let (col, view) = case res of
               DealTsumo {winners, payers} -> (lightBlue,
                  [ asText "Win: " `beside` asText winners
                  , asText "Payers: " `beside` asText payers
                  ])
               DealRon {winners, payers} -> (lightGreen,
                  [ asText "Win: " `beside` asText winners
                  , asText "Payers: " `beside` asText payers
                  ])
               DealDraw {tenpai, nooten} -> (lightYellow,
                  [ asText "Tenpai players: " `beside` asText tenpai
                  , asText "Nooten: " `beside` asText nooten
                  ])
   in toForm
         <| color col
         <| container 700 300 middle
         <| flow down view
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
   <| map (dispTile co << fst)
   <| filter (snd >> isNothing) h.discards

dispPublicMentsu co k h = flow right <| map (dispMentsu co k) h.called

dispMentsu : Controls -> Kaze -> Mentsu -> Element
dispMentsu co k m = case m.from of
   Nothing -> empty -- We display only shouted
   Just s  ->
         -- | TODO: this can be jantou from ron
      let (a :: b :: xs) = map (dispTile co) s.shoutTo
          t            = rotate (degrees 90) <| toForm <| dispTile co s.shoutTile
          shoutTile    = case xs of
             [] -> collage t_h t_w [t]
             _  -> collage t_h (2*t_w) [moveY (t_w / 2) t, moveY (-t_w / 2) t]
      in flow right <| case abs (kazeNth s.shoutFrom - kazeNth k) of
         1 -> a :: b :: shoutTile :: []
         2 -> a :: shoutTile :: b :: []
         3 -> shoutTile :: a :: b :: []
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

shouminkanButtons rs str = findShouminkan rs.myhand
   |> map (\t -> clickable shouminkan.handle (Just t) (buttonElem' str blue))

ankanButton rs str = case findFourTiles rs of
   Just t  -> clickable ankan.handle (Just t) (buttonElem' str green)
   Nothing -> empty

-- TODO is his turn
nocareButton w str = case w of
   Just (w, _ :: _) -> clickable nocare.handle True <| buttonElem' str green
   _                -> empty

findFourTiles : RoundState -> Maybe Tile
findFourTiles rs = counted 4 <| sortTiles <| rs.myhand.concealed ++ maybe [] (\x -> [x]) rs.myhand.pick

findShouminkan h = filter (\x -> x.mentsuKind == Koutsu &&
   (List.any (\t -> t == x.tile) h.concealed || h.pick == Just x.tile)) h.called
   |> map .tile

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
      { gs | waitShout      <- Just <|
         ( WaitRecord seconds gs.updated
         , maybe shouts (\x -> snd x ++ shouts) gs.waitShout)
      }

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
      setResults res gs

   RoundNick {player_kaze, nick} ->
      setNick player_kaze nick gs

-- {{{ Field modify boilerplate ----------------------------------------------
setMyHand hand gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | myhand <- hand } }
   Nothing -> gs

setTurnPlayer player gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | turn <- player } }
   Nothing -> gs

addTurnAction player action gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | actions <- (player, action) :: rs.actions } }
   Nothing -> gs

setPlayerHand player hand gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | hands <- updateHand player hand rs.hands } }
   Nothing -> gs

setNick pk nick gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | players <- Util.listModify pk (\(p, n, _) -> (p, n, nick)) rs.players } }
   Nothing -> gs

setResults res gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | results <- Just res } }
   Nothing -> gs
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
