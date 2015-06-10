module Game where

import Util exposing (..)
import GameTypes exposing (..)

import List
import List exposing (map)
import Array
import Signal exposing ((<~), dropRepeats, mergeMany, Mailbox, mailbox, message)
import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Text as T
import Time exposing (inSeconds)

-- TODO: hard-coded tile widths; some other measures are also hard-coded
t_w = 62
t_h = 82
discards_off = 330
called_off   = 650
riichi_off   = 130

-- {{{ Controls ------------------------------------------------------
type alias Controls =
   { hoveredTile : Maybe Tile }

controls : Signal Controls
controls = Controls <~ dropRepeats discardHover.signal

-- Maybe a tile to discard from my hand
discard : Mailbox (Maybe Discard)
discard = mailbox Nothing
riichi = mailbox Nothing
tsumo  = mailbox Nothing

discardHover : Mailbox (Maybe Tile)
discardHover = mailbox Nothing

shout : Mailbox (Maybe Shout)
shout = mailbox Nothing

ankan : Mailbox (Maybe Tile)
ankan = mailbox Nothing

shouminkan : Mailbox (Maybe Tile)
shouminkan = mailbox Nothing

nocare : Mailbox Bool
nocare = mailbox False

shoutChooseTile : Mailbox (Maybe Tile)
shoutChooseTile = mailbox Nothing
-- }}}

-- {{{ Upstream events ----------------------------------------------------
events : Signal Event
events = mergeMany
   [ maybe Noop (InGameAction << GameTurn << TurnTileDiscard) <~ discard.signal
   , maybe Noop (InGameAction << GameTurn << TurnTileDiscard) <~ riichi.signal
   , maybe Noop (InGameAction << GameShout) <~ shout.signal
   , maybe Noop (InGameAction << GameTurn << TurnAnkan) <~ ankan.signal
   , maybe Noop (InGameAction << GameTurn << TurnShouminkan) <~ shouminkan.signal
   , maybe Noop (\_ -> InGameAction <| GameTurn TurnTsumo) <~ tsumo.signal
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
            , group <| map (fst >> \k -> moveRotateKaze riichi_off rs.mypos k playerRiichi)
                    <| List.filter (snd >> .riichi) rs.hands
            , maybe (toForm empty) dispResults rs.results
            ]
        , container 1000 40 midTop <| flow right
           <| (
              maybe [] (map shoutButton << snd) (gs.waitShout)
               ++ shouminkanButtons rs "Shouminkan"
               ++ [ ankanButton rs "Ankan"
                  , nocareButton gs.waitShout "Pass" ]
               ++ riichiButtons gs.riichiWith
               ++ [ tsumoButton rs.myhand.canTsumo ]
               )
        , container 1000 100 midTop <| dispHand rs.mypos co rs.myhand
        ]
   Nothing -> show "Hmm, roundState is Nothing but I should be in a game"
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
   |> List.filter (\x -> x /= rs.mypos)
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

playerRiichi : Form
playerRiichi = toForm <| image 200 20 "/static/img/point1000.svg"

-- }}}

-- {{{ Info block ----------------------------------------------------
dispInfoBlock co gs rs =
   toForm
   <| color black <| container 234 234 middle
   <| color white <| size 230 230
   <| collage 230 230
   <| [ turnIndicator gs |> moveRotateKaze 90 rs.mypos rs.turn
      , toForm <| (centered <| T.bold <| T.fromString <| toString rs.round) `beside` show (" " ++ toString rs.deal)
      , move (-60, 60) <| scale 0.6 <| toForm <| dispWanpai co rs
      , if rs.honba > 0 then move (-60,-60) <| toForm <| centered <| T.fromString <| toString (rs.honba * 100) ++ "H"
                        else toForm empty
      , moveY (-30) <| toForm <| centered <| T.fromString <| toString rs.tilesleft
      ]
      ++ map (\k -> dispPlayerInfo rs k |> moveRotateKaze 95 rs.mypos k) [Ton, Nan, Shaa, Pei]

turnIndicator : GameState -> Form
turnIndicator gs =
   let col = if isNothing gs.waitShout then lightGreen else lightOrange
                in group
   [ rotate (degrees 90) <| filled col <| ngon 3 80
   , moveY 30
         <| toForm <| show <| floor
         <| case gs.waitTurnAction of
               Nothing -> inSeconds <| gs.updated - gs.turnBegan
               Just wr -> toFloat wr.seconds - (inSeconds (gs.updated - wr.added))
   ]

dispPlayerInfo rs k =
   let (p, points, name) = Util.listFind k rs.players
   in show p `beside` ((if name == "" then T.fromString "(bot)" else T.fromString name |> T.bold) |> centered)
      `above` flow right [ spacer 5 5, show k, spacer 5 5, show points ] |> toForm
-- }}}

-- {{{ Results
dispResults : RoundResult -> Form
dispResults res =
   let (col, view) = case res of
               DealTsumo {winners, payers} -> (lightBlue,
                  [ show "Tsumo! " ]
                  ++ [flow down <| map dispWinner winners]
                  ++ [ show "Payers: " `beside` show payers ])
               DealRon {winners, payers} -> (lightGreen,
                  [ show "Win: " `beside` show winners
                  , show "Payers: " `beside` show payers
                  ])
               DealDraw {tenpai, nooten} -> (lightYellow,
                  [ show "Tenpai players: " `beside` show tenpai
                  , show "Nooten: " `beside` show nooten
                  ])
   in toForm
         <| color col
         <| container 700 300 middle
         <| flow down view

dispWinner : Winner -> Element
dispWinner {player, valuehand} = flow down <|
   [ show player, dispValued valuehand ]

dispValued : Valued -> Element
dispValued {mentsu, tiles, value} = flow right
   (map dispTile tiles ++ map (dispMentsu Ton) mentsu) -- TODO get player ton
   `above` dispHandValue value

dispHandValue : HandValue -> Element
dispHandValue {yaku, fu, han, points, named} = flow right
   [ show han, show " Han, ", show fu, show " Fu." ]
   `above` flow down (map dispYaku yaku)

dispYaku : Yaku -> Element
dispYaku {han, name} = show name `beside` show " " `beside` show han
-- }}}

-- {{{ Hands --------------------------------------------------------------
dispHand k co hand = flow right
   [ flow right <| map (dispTileClickable co) <| sortTiles hand.concealed
   , spacer 10 10
   , maybe empty (dispTileClickable co >> color lightGreen) hand.pick
   , spacer 10 10
   , flow right <| map (dispMentsu k) hand.called
   ]

dispHandDiscards co h = h.discards
   |> List.filter (.to >> isNothing)
   |> Util.groupInto 6
   |> map (map dispDiscard >> flow right)
   |> flow down
   |> container (6*(t_w+4)+2) (3*(t_h+4)) topLeft

dispDiscard : Discard -> Element
dispDiscard d = if d.riichi
   then collage (t_h+4) (t_w+4) [ rotate (degrees 90) <| toForm <| dispTile d.tile ]
   else dispTile d.tile

dispPublicMentsu co k h = flow right <| map (dispMentsu k) h.called

dispMentsu : Kaze -> Mentsu -> Element
dispMentsu k m = case m.from of
   Nothing -> empty -- We display only shouted
   Just s  ->
         -- | TODO: this can be jantou from ron
      let (a :: b :: xs) = map dispTile s.shoutTo
          t            = rotate (degrees 90) <| toForm <| dispTile s.shoutTile
          shoutTile    = case xs of
             [] -> collage t_h t_w [t]
             _  -> collage t_h (2*t_w) [moveY (t_w / 2) t, moveY (-t_w / 2) t]
      in flow right <| case abs (kazeNth s.shoutFrom - kazeNth k) of
         1 -> a :: b :: shoutTile :: []
         2 -> a :: shoutTile :: b :: []
         3 -> shoutTile :: a :: b :: []
-- }}}

-- {{{ Tiles -----------------------------------------------------------------------
dispTile : Tile -> Element
dispTile tile = container (t_w + 4) (t_h + 4) middle
   <| hoverable (\h -> message discardHover.address <| if h then Just tile else Nothing)
   <| size t_w t_h
   <| tileImage tile

dispWanpai : Controls -> RoundState -> Element
dispWanpai co = .dora >> map dispTile >> flow right

dispTileClickable : Controls -> Tile -> Element
dispTileClickable co tile = dispTile tile |> clickable (message discard.address (Just <| Discard tile Nothing False))

tileImage tile =
   let (row, col) = case tile of
               Suited ManTile n _  -> (125 + (n - 1) * 97, 47)
               Suited PinTile n _  -> (125 + (n - 1) * 97, 142)
               Suited SouTile n _  -> (125 + (n - 1) * 97, 237)
               Honor (Kazehai k)   -> (222 + (kazeNth k) * 97, 356)
               Honor (Sangenpai s) -> (707 + (sangenNth s) * 97, 356)
   in
      croppedImage (row, col) 62 82 "static/img/Mahjong-tiles.jpg"
-- }}}

-- {{{ Buttons 'n stuff --------------------------------------------------------

shoutButton : Shout -> Element
shoutButton s =
   clickable (message shout.address (Just s)) <| collage 80 40
      [ oval 80 40 |> filled green
      , toString s.shoutKind |> T.fromString |> centered |> toForm ]
      -- TODO multiple chii identify

shouminkanButtons rs str = findShouminkan rs.myhand
   |> map (\t -> clickable (message shouminkan.address (Just t)) <| buttonElem' str blue)

riichiButtons = map (\t -> clickable (message riichi.address (Just <| Discard t Nothing True)) (buttonElem' "Riichi" red))

ankanButton rs str = case findFourTiles rs of
   Just t  -> clickable (message ankan.address (Just t)) (buttonElem' str green)
   Nothing -> empty

tsumoButton b = if b then clickable (message tsumo.address (Just ())) <| buttonElem' "Tsumo" orange else empty

-- TODO is his turn
nocareButton w str = case w of
   Just (w, _ :: _) -> clickable (message nocare.address True) <| buttonElem' str green
   _                -> empty

findFourTiles : RoundState -> Maybe Tile
findFourTiles rs = counted 4 <| sortTiles <| rs.myhand.concealed ++ maybe [] (\x -> [x]) rs.myhand.pick

findShouminkan h = List.filter (\x -> x.mentsuKind == Koutsu &&
   (List.any (\t -> t == x.tile) h.concealed || h.pick == Just x.tile)) h.called
   |> map .tile

-- | The nth unique tile
counted : Int -> List Tile -> Maybe Tile
counted m ts =
   let go n xs = case xs of
      y :: ys -> if n == 0 then Just y
                           else case ys of
                              z :: zs -> if y == z then go (n - 1) ys
                                                   else counted m ys
                              _       -> Nothing
      _       -> Nothing
   in go m ts

buttonElem' str col = collage 80 40
   [ oval 80 40 |> filled col
   , T.fromString str |> centered |> toForm ]
-- }}}

-- {{{ Process GameEvents
processInGameEvent : GameEvent -> GameState -> GameState
processInGameEvent event gs = case event of
   RoundPrivateStarts rs ->
        { gs | status <- InGame
             , gameWait <- Nothing
             , roundState <- Just rs }

   RoundPrivateWaitForTurnAction {seconds, riichiWith} ->
      { gs | waitTurnAction <- Just <| WaitRecord seconds gs.updated
           , riichiWith     <- riichiWith
        }
   RoundPrivateWaitForShout {seconds, shouts} ->
      { gs | waitShout      <- Just <|
         ( WaitRecord seconds gs.updated
         , maybe shouts (\x -> snd x ++ shouts) gs.waitShout)
      }

   RoundPrivateChange {hand} -> setMyHand hand gs

   RoundTurnBegins {player_kaze} ->
      Util.log ("Turn of " ++ toString player_kaze) gs
      |> setTurnPlayer player_kaze
      |> \gs -> { gs | turnBegan      <- gs.updated
                     , waitTurnAction <- Nothing
                     , waitShout      <- Nothing
                     , riichiWith     <- []
                }

   RoundTurnAction {player_kaze, action} ->
       processTurnAction player_kaze action gs

   RoundTurnShouted {player_kaze, shout} ->
      Util.log (toString player_kaze ++ " shouted: " ++ toString shout) gs

   RoundHandChanged {player_kaze, hand} -> setPlayerHand player_kaze hand gs

   RoundEnded res -> setResults res gs

   RoundNick {player_kaze, nick} -> setNick player_kaze nick gs
   RoundRiichi {player_kaze} -> setRiichi player_kaze gs
   RoundGamePoints {player,points} -> setPoints player points gs
   RoundFlippedDora {tile} -> flipDora tile gs

-- {{{ Field modify boilerplate ----------------------------------------------
setMyHand hand gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | myhand <- hand } }
   Nothing -> gs

setTurnPlayer player gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | turn <- player } }
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

flipDora tile gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | dora <- tile :: rs.dora
                                        , tilesleft <- rs.tilesleft - 1 } }
   Nothing -> gs

setRiichi pk gs = case gs.roundState of
   Just rs -> { gs | roundState <- Just { rs | hands <- Util.listModify pk (\h -> { h | riichi <- True }) rs.hands } }
   Nothing -> gs

setPoints p n gs = case gs.roundState of
   Just rs -> gs -- TODO { gs | roundState <- Just { rs | players <- Util.listModify p (\h -> { h | riichi <- True }) rs.hands } }
   Nothing -> gs

updateHand player hand = Util.listModify player (\_ -> hand)
-- }}}

-- {{{ Turns ------------------------------------------------------------------
processTurnAction : Kaze -> TurnAction -> GameState -> GameState
processTurnAction player action gs =
   case gs.roundState of
      Just rs -> case action of
         TurnTileDiscard discard ->
            { gs | roundState <-
                  Just { rs | hands  <- Util.listModify player
                                          (\h -> { h | discards <- h.discards ++ [discard]
                                                     , riichi <- h.riichi || discard.riichi
                                          }) rs.hands
                       }
            }
         TurnTileDraw _ _ ->
            { gs | roundState <- Just { rs | tilesleft <- rs.tilesleft - 1 }
            }
         TurnAnkan tile  ->
            { gs | roundState <- Just { rs | hands <- Util.listModify player
               (\h -> { h | called <- h.called ++ [ kantsu tile ] }) rs.hands }
            }
         TurnShouminkan tile -> gs
         TurnTsumo           -> gs

kantsu : Tile -> Mentsu
kantsu t = Mentsu Kantsu t Nothing

-- }}}
-- }}}
