module Game where

import Util exposing (..)
import GameTypes exposing (..)

import List
import Time exposing (Time)
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

-- | These are exactly the size in the tile source image, so don't touch. 
t_w = 62
t_h = 82

-- TODO: hard-coded tile widths; some other measures are also hard-coded
discards_off = 330
called_off   = 600
riichi_off   = 130

-- {{{ State, input and events ---------------------------------------

type alias State a =
   { a | hoveredTileNth : Int
       , relatedToShout : List Int
       , logging        : List LogItem
       , status         : Status

       , gameWait   : Maybe Int
       , gameFinalPoints : Maybe FinalPoints
       , riichiWith     : List Tile
       , waitTurnAction : Maybe WaitRecord
       , waitShout      : Maybe (WaitRecord, List Shout)
       , turnBegan      : Time
       , updated        : Time

       , rs             : RoundState
    }

type UserInput = HoverTile Int
               | HoverShoutTiles (List Int)

userInput : Signal UserInput
userInput = Signal.mergeMany
   [ Signal.map HoverTile <| dropRepeats tileHoverStatus.signal
   , Signal.map (fst >> HoverShoutTiles) <| dropRepeats shoutHover.signal ]

stepUserInput : UserInput -> GameState -> GameState
stepUserInput inp st = case inp of
   HoverTile nth -> { st | hoveredTileNth = nth }
   HoverShoutTiles tiles -> { st | relatedToShout = tiles }

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

-- }}}

-- {{{ Modify state: GameEvents --------------------------------------

processInGameEvent : GameEvent -> GameState -> GameState
processInGameEvent ev st = case st.status of
   InGame rs -> processInGameEventWith ev st rs
   _         -> case ev of
      RoundPrivateStarts rs -> processInGameEventWith ev st rs
      _ -> st

processInGameEventWith : GameEvent -> GameState -> RoundState -> GameState
processInGameEventWith event st rs =

   let updateHand player hand = Util.listModify player (\_ -> hand)

       setRs : GameState -> RoundState -> GameState
       setRs st rs = { st | rs = rs, status = InGame rs }
      
   in case event of

      RoundPrivateStarts rs ->
         setRs { st | gameWait = Nothing } rs

      RoundPrivateWaitForTurnAction {seconds, riichiWith} ->
         { st | waitTurnAction = Just <| WaitRecord seconds st.updated
              , riichiWith     = riichiWith }

      RoundPrivateWaitForShout {seconds, shouts} ->
         { st | waitShout      = Just <| ( WaitRecord seconds st.updated , maybe shouts (\x -> snd x ++ shouts) st.waitShout) }

      RoundPrivateChange {hand} -> setRs st { rs | myhand = hand }

      RoundTurnBegins {player_kaze} ->
         { st | turnBegan      = st.updated
              , waitTurnAction = Nothing
              , waitShout      = Nothing
              , riichiWith     = [] }
              |> Util.log ("Turn of " ++ toString player_kaze)
              |> flip setRs { rs | turn = player_kaze }

      RoundTurnAction {player_kaze, action} -> setRs st <| processTurnAction player_kaze action rs

      RoundTurnShouted {player_kaze, shout} ->
         Util.log (toString player_kaze ++ " shouted: " ++ toString shout) st

      RoundHandChanged {player_kaze, hand} ->
         setRs st { rs | hands = updateHand player_kaze hand rs.hands }

      RoundEnded res ->
         setRs st { rs | results = Just res }

      RoundNick {player_kaze, nick} ->
         setRs st { rs | players = Util.listModify player_kaze (\(p, n, _) -> (p, n, nick)) rs.players }

      RoundRiichi {player_kaze} ->
         setRs st { rs | hands = Util.listModify player_kaze (\h -> { h | riichiState = Riichi }) rs.hands }
         -- XXX: does not differentiate from doubleriichi

      RoundGamePoints {player_kaze,points} -> st
         -- TODO: these are updated in the new kyoku when it starts; but riichi bets are not updated atm (i think)

      RoundFlippedDora {tile} ->
         setRs st { rs | dora = tile :: rs.dora
                  , tilesleft = rs.tilesleft - 1 }

      GameEnded finalPoints -> { st | gameFinalPoints = Just finalPoints }

processTurnAction : Kaze -> TurnAction -> RoundState -> RoundState
processTurnAction player action rs = case action of
    TurnTileDiscard discard ->
       { rs | hands  = Util.listModify player
                           (\h -> { h | discards = h.discards ++ [discard]
                                      , riichiState = if discard.riichi then Riichi else h.riichiState }) rs.hands }
    TurnTileDraw _ _ ->
       { rs | tilesleft = rs.tilesleft - 1 }

    TurnAnkan tile  ->
       { rs | hands = Util.listModify player (\h -> { h | called = h.called ++ [ kantsu tile ] }) rs.hands }

    TurnShouminkan tile -> rs

    TurnTsumo           -> rs

-- }}}

-- {{{ Mailboxes ----------------------------------------------------------

tileHoverStatus : Mailbox Int
tileHoverStatus = mailbox (-1) -- | Tiles are numbered from 1 onwards.

shoutHover : Mailbox (List Int, Maybe Shout) -- (tiles activated, hovered shout)
shoutHover = mailbox ([], Nothing)

-- Maybe a tile to discard from my hand
discard : Mailbox (Maybe Discard)
discard = mailbox Nothing

riichi = mailbox Nothing
tsumo  = mailbox Nothing

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

-- {{{ Display: main -------------------------------------------------

display : State a -> Element
display st = flow down
    [ container 1000 800 midTop <| collage 1000 800
        [ dispInfoBlock st
        , dispDiscards st |> scale 0.7
        , dispOthersHands st |> scale 0.6
        , group <| map (fst >> \k -> moveRotateKaze riichi_off st.rs.mypos k playerRiichi)
                <| List.filter (snd >> .riichiState >> \x -> x /= NoRiichi) st.rs.hands
        , maybe (toForm empty) dispResults st.rs.results
        ]
    , container 1000 40 midTop <| flow right
       <| (
          maybe [] (snd >> displayShoutButtons st) (st.waitShout)
           ++ shouminkanButtons st.rs "Shouminkan"
           ++ [ ankanButton st.rs "Ankan"
              , nocareButton st.waitShout "Pass" ]
           ++ riichiButtons st.rs st.riichiWith
           ++ [ tsumoButton st.rs.myhand.canTsumo ]
           )
    , container 1000 100 midTop <| dispHand st.rs.mypos st st.rs.myhand
    ]

-- }}}

-- {{{ Display: per-player -------------------------------------------

dispOthersHands : State a -> Form
dispOthersHands st = [Ton, Nan, Shaa, Pei]
   |> List.filter (\x -> x /= st.rs.mypos)
   |> map (\k ->
      let maybeTenpai = st.rs.results `Maybe.andThen` (\res -> case res of
            DealDraw{tenpai} -> Just tenpai
            _                -> Nothing) `Maybe.andThen` Util.listFindWith .player_kaze k

      in (case maybeTenpai of
            Just {mentsu,tiles} -> flow right <| map dispTile tiles ++ map (dispMentsu k) mentsu
            Nothing             -> Util.listFind k st.rs.hands |> dispOthersHand st k
         ) |> toForm |> moveRotateKaze called_off st.rs.mypos k
      )
   |> group

dispDiscards : State a -> Form
dispDiscards st = group <| map
   (\k -> Util.listFind k st.rs.hands
       |> dispHandDiscards st
       |> toForm
       |> moveRotateKaze discards_off st.rs.mypos k)
   [Ton, Nan, Shaa, Pei]

-- }}}

-- {{{ Display: info block in the center -----------------------------
dispInfoBlock : State a -> Form
dispInfoBlock st =
   toForm
   <| color black <| container 234 234 middle
   <| color white <| size 230 230
   <| collage 230 230
   <| [ turnIndicator st |> moveRotateKaze 90 st.rs.mypos st.rs.turn
      , dealAndRoundIndicator st.rs
      , toForm (dispWanpai st.rs) |> scale 0.6 |> move (-60, 60)
      , honbaIndicator st.rs.honba
      , riichiInTableIndicator st.rs.inTable
      , moveY (-30) <| toForm <| centered <| T.fromString <| toString st.rs.tilesleft
      ]
      ++ map (\k -> dispPlayerInfo st.rs k |> moveRotateKaze 95 st.rs.mypos k) [Ton, Nan, Shaa, Pei]

dealAndRoundIndicator rs = toForm <| kazeImage (rs.round.kaze) `beside` centered (T.fromString (toString <| rs.round.round_rot))

turnIndicator : State a -> Form
turnIndicator st =
   let col = if isNothing st.waitShout then lightGreen else lightOrange
                in group
   [ rotate (degrees 90) <| filled col <| ngon 3 80
   , moveY 30
         <| toForm <| show <| floor
         <| case st.waitTurnAction of
               Nothing -> inSeconds <| st.updated - st.turnBegan
               Just wr -> toFloat wr.seconds - (inSeconds (st.updated - wr.added))
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

-- {{{ Display: Utility ----------------------------------------------
kazeImage kaze = fittedImage 28 38 <| case kaze of
   Ton  -> "/static/img/Ton.jpg"
   Nan  -> "/static/img/Nan.jpg"
   Shaa -> "/static/img/Shaa.jpg"
   Pei  -> "/static/img/Pei.jpg"

titleText : String -> Element
titleText txt = T.fromString txt |> T.height 40 |> centered

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

-- {{{ Display: Results ----------------------------------------------
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

dispWinner : Winner -> Element
dispWinner {player_kaze, points, valuehand} =
   [ T.concat [ T.fromString (toString player_kaze)
              , T.fromString (toString points) |> T.append (T.fromString "+") |> T.color green
              ] |> centered
   , dispValued valuehand ]
   |> flow down

dispPayer : Payer -> Element
dispPayer {player_kaze, points} = T.concat
   [ T.fromString (toString player_kaze), T.fromString "        "
   , T.fromString (toString points) |> T.append (T.fromString "-") |> T.color red ]
   |> centered

dispTenpai : Tenpai -> Element
dispTenpai {player_kaze, points} = T.concat
   [ T.fromString (toString player_kaze), T.fromString "        "
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

-- {{{ Display: My hand ---------------------------------------------------

dispHand : Kaze -> State a -> Hand -> Element
dispHand k st hand = flow right <|
   List.map2 (dispTileInHand st) [1..14] (sortTiles hand.concealed)
   ++ [ spacer 10 10 ]
   ++ map (pickedTile >> dispTileInHand st (List.length hand.concealed + 1) >> color lightGreen) hand.picks
   ++ [ spacer 10 10 ]
   ++ map (dispMentsu k) hand.called

dispTileInHand : State a -> Int -> Tile -> Element
dispTileInHand st n tile = dispTile tile
   |> clickable (message discard.address (Just <| Discard tile Nothing False))
   |> hoverable (\f -> message tileHoverStatus.address <| if f then n else -1 )
   |> if st.hoveredTileNth == n || List.member n st.relatedToShout then color red else identity

-- }}}

-- {{{ Display: Hand: Discards

dispHandDiscards st h = h.discards
   |> List.filter (.to >> isNothing)
   |> Util.groupInto 6
   |> map (map dispDiscard >> flow right)
   |> flow down
   |> container (6*(t_w+4)+2) (3*(t_h+4)) topLeft

dispDiscard : Discard -> Element
dispDiscard d = if d.riichi
   then collage (t_h+4) (t_w+4) [ rotate (degrees 90) <| toForm <| dispTile d.tile ]
   else dispTile d.tile

-- }}}

-- {{{ Display: Hand: Others

-- | Tiles hidden in other's hands. XXX: Doesn't show the picked tile.
dispHiddenTiles : HandPublic -> Element
dispHiddenTiles h =
   let num = 13 - 2 * List.length h.called
       in flow right <| List.repeat num tileHidden

dispOthersHand : State a -> Kaze -> HandPublic -> Element
dispOthersHand st k h = dispHiddenTiles h `beside` dispPublicMentsu st k h

-- }}}

-- {{{ Display: Mentsu ----------------------------------------------------------------

-- | The first argument tells who we are. We need to know in order to rotate the
-- correct tile in a shouted mentsu.
dispMentsu : Kaze -> Mentsu -> Element
dispMentsu k m = case m.from of
   Nothing -> dispMentsuConcealed m
   Just s  -> dispShout k m s

dispMentsuConcealed : Mentsu -> Element
dispMentsuConcealed m = flow right <| map dispTile <| m.tiles

dispPublicMentsu : State a -> Kaze -> HandPublic -> Element
dispPublicMentsu st k h = flow right <| map (dispMentsu k) h.called

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

-- }}}

-- {{{ Display: Tiles --------------------------------------------------------------

dispTile : Tile -> Element
dispTile tile = container (t_w + 4) (t_h + 4) middle
   <| size t_w t_h
   <| layers [ tileImage tile, if isAka tile then akaIndicator else empty ]

akaIndicator : Element
akaIndicator = collage 20 20 [ oval 20 20 |> filled red ]

dispWanpai : RoundState -> Element
dispWanpai = .dora >> map dispTile >> flow right

tileImage tile =
   let (row, col) = case tile of
               Suited ManTile n _  -> (125 + (n - 1) * 97, 47)
               Suited PinTile n _  -> (125 + (n - 1) * 97, 142)
               Suited SouTile n _  -> (125 + (n - 1) * 97, 237)
               Honor (Kazehai k)   -> (222 + (kazeNth k) * 97, 356)
               Honor (Sangenpai s) -> (707 + (sangenNth s) * 97, 356)
   in
      croppedImage (row, col) 62 82 "/static/img/Mahjong-tiles.jpg"

tileHidden : Element
tileHidden = container 54 34 middle <| collage 54 34 [ rect 50 30 |> outlined defaultLine ]

-- }}}

-- {{{ Buttons -----------------------------------------------------------------

displayShoutButtons : State a -> List Shout -> List Element
displayShoutButtons st = map <| \s -> shoutButton s
   |> hoverable (\f -> message shoutHover.address <|
         if f then (getTileIndices (sortTiles st.rs.myhand.concealed) s.shoutTo, Just s) else ([], Nothing))

riichiButtons : RoundState -> List Tile -> List Element
riichiButtons rs = map <| \t -> buttonElem' "Riichi" red
   |> hoverable (\f -> message shoutHover.address (if f then getTileIndices (sortTiles rs.myhand.concealed) [t] else [], Nothing))
   |> clickable (message riichi.address (Just <| Discard t Nothing True))

shoutButton : Shout -> Element
shoutButton s =
   clickable (message shout.address (Just s)) <| collage 80 40
      [ oval 80 40 |> filled green
      , toString s.shoutKind |> T.fromString |> centered |> toForm ]
      -- TODO multiple chii identify

shouminkanButtons rs str = findShouminkan rs.myhand
   |> map (\t -> clickable (message shouminkan.address (Just t)) <| buttonElem' str blue)

ankanButton rs str = case findFourTiles rs of
   Just t  -> clickable (message ankan.address (Just t)) (buttonElem' str green)
   Nothing -> empty

tsumoButton b = if b then clickable (message tsumo.address (Just ())) <| buttonElem' "Tsumo" orange else empty

-- TODO is his turn
nocareButton w str = case w of
   Just (w, _ :: _) -> clickable (message nocare.address True) <| buttonElem' str green
   _                -> empty

buttonElem' str col = collage 80 40
   [ oval 80 40 |> filled col
   , T.fromString str |> centered |> toForm ]

-- }}}

-- {{{ Utility -----------------------------------------------------------------
findFourTiles : RoundState -> Maybe Tile
findFourTiles rs = counted 4 <| sortTiles <| rs.myhand.concealed ++ map pickedTile rs.myhand.picks

findShouminkan : Hand -> List Tile
findShouminkan h = List.filter
   (\mentsu -> mentsu.mentsuKind == Koutsu && List.any (\t -> t `List.member` mentsu.tiles) (h.concealed ++ map pickedTile h.picks))
   h.called -- TODO This fails when picked more than one tile
     |> map (fromJust << List.head << .tiles)

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

-- | TODO this discards aka-dora info 
kantsu : Tile -> Mentsu
kantsu t = Mentsu Kantsu [t, t, t, t] Nothing

getShout : Maybe ShoutKind -> Maybe Shout
getShout = maybe Nothing (\k -> Just <| Shout k Ton (Suited ManTile 1 False) [])

getTileIndices : List Tile -> List Tile -> List Int
getTileIndices =
   let go n xs is = case (xs, is) of
            (x :: xs, i :: is) -> if x == i then n :: go (n + 1) xs is
                                            else go (n + 1) xs (i :: is)
            _                  -> []
       in go 1
-- }}}
