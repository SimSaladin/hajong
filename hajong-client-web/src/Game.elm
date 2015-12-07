module Game where

import Util exposing (..)
import GameTypes exposing (..)

import List exposing (map)
import Array
import Signal
import Signal exposing (dropRepeats, mergeMany, Mailbox, mailbox, message)
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
controls = Signal.map Controls <| dropRepeats discardHover.signal

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
   [ maybe Noop (InGameAction << GameTurn << TurnTileDiscard) `Signal.map` discard.signal
   , maybe Noop (InGameAction << GameTurn << TurnTileDiscard) `Signal.map` riichi.signal
   , maybe Noop (InGameAction << GameShout)                   `Signal.map` shout.signal
   , maybe Noop (InGameAction << GameTurn << TurnAnkan)       `Signal.map` ankan.signal
   , maybe Noop (InGameAction << GameTurn << TurnShouminkan)  `Signal.map` shouminkan.signal
   , maybe Noop (\_ -> InGameAction <| GameTurn TurnTsumo)    `Signal.map` tsumo.signal
   , (\x -> if x then InGameAction GameDontCare else Noop)    `Signal.map` nocare.signal
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
                    <| List.filter (snd >> .riichiState >> \x -> x /= NoRiichi) rs.hands
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

-- | `movRotateKaze off me him this` rotates a form 'this' of 'him' to correct direction
-- when looked from 'me', and applies an offset of 'off' pixels in his direction.
-- think: Discard pools, etc.
moveRotateKaze : Float -> Kaze -> Kaze -> Form -> Form
moveRotateKaze off mypos pos =
   case (kazeNth pos - kazeNth mypos) % 4 of
      0 -> moveY (-off)
      1 -> moveX off    << rotate (degrees 90) 
      2 -> moveY off    << rotate (degrees 180)
      _ -> moveX (-off) << rotate (degrees 270)

-- | Riichi stick, not rotated.
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
      , dealAndRoundIndicator rs
      , toForm (dispWanpai co rs) |> scale 0.6 |> move (-60, 60)
      , honbaIndicator rs.honba
      , riichiInTableIndicator rs.inTable
      , moveY (-30) <| toForm <| centered <| T.fromString <| toString rs.tilesleft
      ]
      ++ map (\k -> dispPlayerInfo rs k |> moveRotateKaze 95 rs.mypos k) [Ton, Nan, Shaa, Pei]

dealAndRoundIndicator rs = toForm <| kazeImage rs.round `beside` centered (T.fromString (toString rs.deal))

kazeImage kaze = fittedImage 28 38 <| case kaze of
   Ton  -> "/static/img/Ton.jpg"
   Nan  -> "/static/img/Nan.jpg"
   Shaa -> "/static/img/Shaa.jpg"
   Pei  -> "/static/img/Pei.jpg"

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

honbaIndicator h = if h > 0 then move (-60,-60) <| toForm <| centered <| T.fromString <| toString h ++ "H"
                            else toForm empty

riichiInTableIndicator n = if n > 0 then move (-30, -60) <| toForm <| centered <| T.color red <| T.fromString <| toString (n / 1000) ++ "R"
                                    else toForm empty

dispPlayerInfo : RoundState -> Kaze -> Form
dispPlayerInfo rs k =
   let (p, points, name) = Util.listFind k rs.players
       playerName        = if name == "" then T.fromString "(bot)" else T.fromString name |> T.bold
   in
      toForm <| kazeImage k `beside` spacer 5 5 `beside` (centered playerName `above` show points)
-- }}}

-- {{{ Results
dispResults : RoundResult -> Form
dispResults res =
   let (col, view) = case res of
            DealTsumo {winners, payers} -> (lightBlue,
               titleText "Tsumo!"
               :: flow down (map dispWinner winners)
               :: flow down (map dispPayer payers) :: [])

            DealRon {winners, payers} -> (lightGreen,
               titleText "Ron!"
               :: flow down (map dispWinner winners)
               :: flow down (map dispPayer payers) :: [])

            DealDraw {tenpai, nooten} -> (lightYellow,
               titleText "Draw!"
               :: flow down (map dispTenpai tenpai)
               :: flow down (map dispPayer nooten) :: [])
   in toForm
         <| color col
         <| container 700 300 middle
         <| flow down view

titleText : String -> Element
titleText txt = T.fromString txt |> T.height 40 |> centered

dispWinner : Winner -> Element
dispWinner {player, valuehand} =
   [ T.concat [ T.fromString (toString player)
              , T.fromString (toString valuehand.value.points) |> T.append (T.fromString "+") |> T.color green
              ] |> centered
   , dispValued valuehand ]
   |> flow down

dispPayer : Payer -> Element
dispPayer {player, points} = T.concat
   [ T.fromString (toString player), T.fromString "        "
   , T.fromString (toString points) |> T.append (T.fromString "-") |> T.color red ]
   |> centered

dispTenpai : Payer -> Element
dispTenpai {player, points} = T.concat
   [ T.fromString (toString player), T.fromString "        "
   , T.fromString (toString points) |> T.append (T.fromString "+") |> T.color green ]
   |> centered

dispValued : Valued -> Element
dispValued {mentsu, tiles, value} =
   [ collage 600 80
      [ flow right (map dispTile tiles ++ map (dispMentsu Ton) mentsu) -- TODO get player ton
        |> toForm |> scale 0.6 ]
   , dispHandValue value
   ] |> flow down

dispHandValue : HandValue -> Element
dispHandValue {yaku, fu, han, points, named} = (T.concat
   [ T.fromString (toString han), T.fromString " Han, "
   , T.fromString (toString fu), T.fromString (" Fu.") ]
   |> centered)
   `above` flow down (map dispYaku yaku)

dispYaku : Yaku -> Element
dispYaku {han, name} = T.concat
   [ T.fromString name |> T.bold, T.fromString " "
   , T.fromString (toString han) |> T.color green ]
   |> centered
-- }}}

-- {{{ Hands --------------------------------------------------------------
dispHand k co hand = flow right
   [ flow right <| map (dispTileClickable co) <| sortTiles hand.concealed
   , spacer 10 10
   , flow right <| map (pickedTile >> dispTileClickable co >> color lightGreen) hand.picks
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

-- | The first argument tells who we are. We need to know in order to rotate the
-- correct tile in a shouted mentsu.
dispMentsu : Kaze -> Mentsu -> Element
dispMentsu k m = case m.from of
   Nothing -> dispMentsuConcealed m
   Just s  -> dispShout k m s

dispShout : Kaze -> Mentsu -> Shout -> Element
dispShout k m s =
   let myTiles  = map dispTile s.shoutTo
       rotated  = rotate (degrees 90) <| toForm <| dispTile s.shoutTile
       shoutPos =
          if s.shoutKind == Pon && m.mentsuKind == Kantsu -- shouminkan?
             then collage t_h (2*t_w) [moveY (t_w / 2) rotated, moveY (-t_w / 2) rotated]
             else collage t_h t_w [rotated]

   in flow right <| case abs (kazeNth s.shoutFrom - kazeNth k) of
      1 -> List.take 2 myTiles ++ [shoutPos] ++ List.drop 2 myTiles
      2 -> List.take 1 myTiles ++ [shoutPos] ++ List.drop 1 myTiles
      _ -> shoutPos :: myTiles

dispMentsuConcealed : Mentsu -> Element
dispMentsuConcealed m = Debug.crash "TODO implement concealed mentsu display"
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
      croppedImage (row, col) 62 82 "/static/img/Mahjong-tiles.jpg"
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
findFourTiles rs = counted 4 <| sortTiles <| rs.myhand.concealed ++ map pickedTile rs.myhand.picks

findShouminkan h = List.filter (\x -> x.mentsuKind == Koutsu &&
   (List.any (\t -> t == x.tile) h.concealed || map pickedTile h.picks == [x.tile])) h.called -- TODO This fails when picked more than one tile
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
        { gs | status = InGame
             , gameWait = Nothing
             , roundState = Just rs }

   RoundPrivateWaitForTurnAction {seconds, riichiWith} ->
      { gs | waitTurnAction = Just <| WaitRecord seconds gs.updated
           , riichiWith     = riichiWith
        }
   RoundPrivateWaitForShout {seconds, shouts} ->
      { gs | waitShout      = Just <|
         ( WaitRecord seconds gs.updated
         , maybe shouts (\x -> snd x ++ shouts) gs.waitShout)
      }

   RoundPrivateChange {hand} -> setMyHand hand gs

   RoundTurnBegins {player_kaze} ->
      Util.log ("Turn of " ++ toString player_kaze) gs
      |> setTurnPlayer player_kaze
      |> \gs -> { gs | turnBegan      = gs.updated
                     , waitTurnAction = Nothing
                     , waitShout      = Nothing
                     , riichiWith     = []
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
   Just rs -> { gs | roundState = Just { rs | myhand = hand } }
   Nothing -> gs

setTurnPlayer player gs = case gs.roundState of
   Just rs -> { gs | roundState = Just { rs | turn = player } }
   Nothing -> gs

setPlayerHand player hand gs = case gs.roundState of
   Just rs -> { gs | roundState = Just { rs | hands = updateHand player hand rs.hands } }
   Nothing -> gs

setNick pk nick gs = case gs.roundState of
   Just rs -> { gs | roundState = Just { rs | players = Util.listModify pk (\(p, n, _) -> (p, n, nick)) rs.players } }
   Nothing -> gs

setResults res gs = case gs.roundState of
   Just rs -> { gs | roundState = Just { rs | results = Just res } }
   Nothing -> gs

flipDora tile gs = case gs.roundState of
   Just rs -> { gs | roundState = Just { rs | dora = tile :: rs.dora
                                        , tilesleft = rs.tilesleft - 1 } }
   Nothing -> gs

setRiichi pk gs = case gs.roundState of -- TODO does not differentiate from doubleriichi
   Just rs -> { gs | roundState = Just { rs | hands = Util.listModify pk (\h -> { h | riichiState = Riichi }) rs.hands } }
   Nothing -> gs

setPoints p n gs = case gs.roundState of
   Just rs -> gs -- TODO { gs | roundState = Just { rs | players = Util.listModify p (\h -> { h | riichi = True }) rs.hands } }
   Nothing -> gs

updateHand player hand = Util.listModify player (\_ -> hand)
-- }}}

-- {{{ Turns ------------------------------------------------------------------
processTurnAction : Kaze -> TurnAction -> GameState -> GameState
processTurnAction player action gs =
   case gs.roundState of
      Nothing -> gs
      Just rs -> case action of
         TurnTileDiscard discard ->
            { gs | roundState =
                  Just { rs | hands  = Util.listModify player
                                          (\h -> { h | discards = h.discards ++ [discard]
                                                     , riichiState = if discard.riichi then Riichi else h.riichiState
                                          }) rs.hands
                       }
            }
         TurnTileDraw _ _ ->
            { gs | roundState = Just { rs | tilesleft = rs.tilesleft - 1 }
            }
         TurnAnkan tile  ->
            { gs | roundState = Just { rs | hands = Util.listModify player
               (\h -> { h | called = h.called ++ [ kantsu tile ] }) rs.hands }
            }
         TurnShouminkan tile -> gs
         TurnTsumo           -> gs

kantsu : Tile -> Mentsu
kantsu t = Mentsu Kantsu t Nothing

-- }}}
-- }}}
