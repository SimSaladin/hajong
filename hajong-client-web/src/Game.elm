module Game where

import Util exposing (..)
import Lounge
import GameTypes exposing (..)
import MsgDialog

import List
import Dict exposing (Dict)
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

import Html

import Debug

-- Dimensions
t_w = 31 -- tile width, in source image
t_h = 47 -- tile height
offSpacer = 5

infoBlockWidth = 6 * t_w - 20
riichiBetsOffCenter = 3 * t_w - 3
discardsOffCenter = 3 * t_w + offSpacer
wallOffCenter = 3 * t_w + 3 * t_h + 2 * offSpacer
handOffCenter = 3 * t_w + 4 * t_h + 3 * offSpacer
playAreaWidth = 2 * handOffCenter + 2 * t_h
playAreaHeight = playAreaWidth

-- {{{ State, input and events ---------------------------------------

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
         setRs { st | gameWait = Nothing, gameFinalPoints = Nothing } rs

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
         { st | relatedToShout = [], hoveredTileNth = 0 }
         |> Util.log (toString player_kaze ++ " shouted: " ++ toString shout)

      RoundHandChanged {player_kaze, hand} ->
         setRs { st | hoveredTileNth = -1 }
               { rs | hands = updateHand player_kaze hand rs.hands }

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

display : GameState -> Element
display st =
   let propY = toFloat (fst st.dimensions) / playAreaHeight
       propX = toFloat (snd st.dimensions) / playAreaWidth
       factor = min 1 <| min propY propX
       x     = Debug.watch "Dimensions" <| st.dimensions
       realGameArea = min playAreaWidth <| min (fst st.dimensions) (snd st.dimensions)
       gameDisplay = collage realGameArea realGameArea
            [ scale factor <| group
                [ dispInfoBlock st
                , dispDiscards st
                , dispOthersHands st
                , dispPlayerInfos st

                , group <| map (fst >> \k -> moveRotateKaze (riichiBetsOffCenter, 0) st.rs.mypos k (playerRiichi st))
                        <| List.filter (snd >> .riichiState >> \x -> x /= NoRiichi) st.rs.hands

                , maybe (toForm empty) (dispResults st) st.rs.results
                , maybe (toForm empty) (dispFinalPoints st) st.gameFinalPoints

                -- action buttons
                , moveY (-handOffCenter + t_h) <| toForm <| container playAreaWidth 40 midTop <| displayActionButtons st
                ]
             , dispHand st.rs.mypos st st.rs.myhand
                  |> fitToViewX st.dimensions
                  |> moveY (-handOffCenter * factor)
             ] |> color (rgb 210 207 190)

   in container (fst st.dimensions) (snd st.dimensions) middle gameDisplay
         |> color gray

-- TODO: Side-bar not done yet.
asSideBar : GameState -> Element
asSideBar st = container 300 (snd st.dimensions) middle
   <| color black
   <| MsgDialog.dialog st

-- | Shouts and so on, as boxes above own hand.
displayActionButtons : GameState -> Element
displayActionButtons st = flow right <|
   -- shout-related
   maybe [] (snd >> displayShoutButtons st) (st.waitShout) ++ [ nocareButton st.waitShout "Pass" ]
   -- own turn
   ++ if st.rs.turn == st.rs.mypos
         then shouminkanButtons st.rs "Shouminkan"
              ++ [ ankanButton st.rs "Ankan" ]
              ++ riichiButtons st.rs st.riichiWith
              ++ [ tsumoButton st.rs.myhand.canTsumo ]
         else []

fitToViewX : (Int, Int) -> Element -> Form
fitToViewX (w,_) e = scale (min 1 <| toFloat w / toFloat (widthOf e)) <| toForm e

dispPlayerInfos : GameState -> Form
dispPlayerInfos st = [Ton, Nan, Shaa, Pei]
   |> List.concatMap (\k ->
      [ dispPlayerInfo st k |> moveRotateKaze (handOffCenter-t_h,-playAreaWidth / 4) st.rs.mypos k
      , kazeImage st k |> toForm |> scale 0.7 |> moveRotateKaze (2.5 * t_w, 0) st.rs.mypos k ]
   )
   |> group

dispPlayerInfo : GameState -> Kaze -> Form
dispPlayerInfo st k =
   let (p, points, name) = Util.listFind k st.rs.players
       displayName       = getInfoWithDefault st name .displayName name
       playerName        = if name == "" then T.fromString "bot"
                                         else T.fromString displayName |> T.bold
   in
      Html.toElement 30 30 (Lounge.smallPlayerInfo st name)
         `beside` spacer 5 5
         `beside` (centered playerName `above` show points)
         |> container infoBlockWidth 50 topLeft
         |> toForm

-- }}}

-- {{{ DIsplay: FinalPoints screen ----------------------------------

dispFinalPoints : GameState -> FinalPoints -> Form
dispFinalPoints st res =
   let dispPointsFor (player, points) =
         let (_, (_, _, nick)) = fromJust <| Util.listFindWith (\(_,(p,_,_)) -> p) player st.rs.players
         in show nick `beside` spacer 5 5 `beside` show points
   in List.map dispPointsFor res
      |> flow down
      |> container 400 300 middle
      |> color green
      |> toForm

-- }}}

-- {{{ Display: Public Hand Info -------------------------------------

dispOthersHands : GameState -> Form
dispOthersHands st = [Ton, Nan, Shaa, Pei]
   |> List.filter (\x -> x /= st.rs.mypos)
   |> map (\k ->
      let maybeTenpai = st.rs.results `Maybe.andThen` (\res -> case res of
            DealDraw{tenpai} -> Just tenpai
            _                -> Nothing) `Maybe.andThen` Util.listFindWith .player_kaze k

      in (case maybeTenpai of
            Just {mentsu,tiles} -> flow right <| map (dispTile st) tiles ++ map (dispMentsu st k) mentsu
            Nothing             -> Util.listFind k st.rs.hands |> dispOthersHand st k
         ) |> toForm |> moveRotateKaze (handOffCenter, 0) st.rs.mypos k
      )
   |> group

dispDiscards : GameState -> Form
dispDiscards st = group <| map
   (\k -> Util.listFind k st.rs.hands
       |> dispHandDiscards st
       |> \e -> toForm e
       |> moveRotateKaze (discardsOffCenter + toFloat (heightOf e) / 2, 0.5 * infoBlockWidth) st.rs.mypos k
    ) [Ton, Nan, Shaa, Pei]

-- }}}

-- {{{ Display: info block in the center -----------------------------

dispInfoBlock : GameState -> Form
dispInfoBlock st =
   toForm
   <| color black <| container (infoBlockWidth + 2) (infoBlockWidth + 2) middle
   <| color gray
   <| collage infoBlockWidth infoBlockWidth
   <| [ turnIndicator st |> moveRotateKaze (infoBlockWidth / 2, 0) st.rs.mypos st.rs.turn
      , dealAndRoundIndicator st
      , toForm (dispWanpai st) |> move (-40, 50)
      , honbaIndicator st.rs.honba
      , riichiInTableIndicator st.rs.inTable
      , moveY (-t_h) <| toForm <| centered <| T.fromString <| toString st.rs.tilesleft
      ]

dealAndRoundIndicator st = toForm
   <| kazeImage st (st.rs.round.kaze)
         `beside`
      centered (T.fromString (toString <| st.rs.round.round_rot))

turnIndicator : GameState -> Form
turnIndicator st =
   let col = if isNothing st.waitShout then lightGreen else lightOrange
                in group
   [ rotate (degrees 90) <| filled col <| ngon 3 80
   , moveY 60
         <| toForm <| show <| floor
         <| case st.waitTurnAction of
               Nothing -> inSeconds <| st.updated - st.turnBegan
               Just wr -> toFloat wr.seconds - (inSeconds (st.updated - wr.added))
   ]

honbaIndicator h = if h > 0 then move (-60,-60) <| toForm <| centered <| T.fromString <| toString h ++ "H"
                            else toForm empty

riichiInTableIndicator n = if n > 0 then move (-30, -60) <| toForm <| centered <| T.color red <| T.fromString <| toString (n / 1000) ++ "R"
                                    else toForm empty
-- }}}

-- {{{ Display: Utility ----------------------------------------------
kazeImage : GameState -> Kaze -> Element
kazeImage st kaze = fittedImage 28 38 <| lookupResource st (toString kaze)

titleText : String -> Element
titleText txt = T.fromString txt |> T.height 40 |> centered

-- | `movRotateKaze off me him this` rotates a form 'this' of 'him' to correct direction
-- when looked from 'me', and applies an offset of 'off' pixels in his direction.
-- think: Discard pools, etc.
moveRotateKaze : (Float, Float) -> Kaze -> Kaze -> Form -> Form
moveRotateKaze (offY, offX) mypos pos =
   case (kazeNth pos - kazeNth mypos) % 4 of
      0 -> moveY (-offY) << moveX offX
      1 -> moveX offY    << moveY offX << rotate (degrees 90) 
      2 -> moveY offY    << moveX (-offX) << rotate (degrees 180)
      _ -> moveX (-offY) << moveY (-offX) << rotate (degrees 270)

-- | Riichi stick, not rotated.
playerRiichi : GameState -> Form
playerRiichi st = toForm <| image (infoBlockWidth - 20) 20 <| lookupResource st "stick-1000"
-- }}}

-- {{{ Display: Results ----------------------------------------------
dispResults : GameState -> RoundResult -> Form
dispResults st res =
   let (col, view) = case res of
            DealTsumo {winners, payers} -> (lightBlue,
               titleText "Tsumo!"
               :: flow down (map (dispWinner st) winners)
               :: flow down (map dispPayer payers) :: [])

            DealRon {winners, payers} -> (lightGreen,
               titleText "Ron!"
               :: flow down (map (dispWinner st) winners)
               :: flow down (map dispPayer payers) :: [])

            DealDraw {tenpai, nooten} -> (lightYellow,
               titleText "Draw!"
               :: flow down (map dispTenpai tenpai)
               :: flow down (map dispPayer nooten) :: [])
   in toForm
         <| color col
         <| container 700 300 middle
         <| flow down view

dispWinner : GameState -> Winner -> Element
dispWinner st {player_kaze, points, valuehand} =
   [ T.concat [ T.fromString (toString player_kaze)
              , T.fromString (toString points) |> T.append (T.fromString "+") |> T.color green
              ] |> centered
   , dispValued st valuehand ]
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

dispValued : GameState -> Valued -> Element
dispValued st {mentsu, tiles, value} =
   [ collage 600 80
      [ flow right (map (dispTileHand st) tiles ++ map (dispMentsu st Ton) mentsu) -- TODO get player ton
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

dispHand : Kaze -> GameState -> Hand -> Element
dispHand k st hand = flow right <|
   List.map2 (dispTileInHand st) [1..14] (sortTiles hand.concealed)
   ++ [ spacer 10 10 ]
   ++ map (pickedTile >> dispTileInHand st (List.length hand.concealed + 1)) hand.picks
   ++ [ spacer 10 10 ]
   ++ map (dispMentsu st k) hand.called

-- | Adds click events and hover effects.
dispTileInHand : GameState -> Int -> Tile -> Element
dispTileInHand st n tile =
   let isHover = st.hoveredTileNth == n || List.member n st.relatedToShout
       in dispTileHand st tile
   |> hoverable (\f -> message tileHoverStatus.address <| if f then n else -1 )
   |> \telem -> container t_w t_h middle (if isHover then opacity 0.7 telem else telem)
   |> clickable (message discard.address (Just <| Discard tile Nothing False))
   |> if isHover then color blue else identity

-- }}}

-- {{{ Display: Hand: Discards

-- Discard pool height: 3 * t_h
--              width:  2 * infoBlockWidth (last row might continue further)
dispHandDiscards : GameState -> HandPublic' b -> Element
dispHandDiscards st h =
   let f i disc = move
   in h.discards |> List.filter (.to >> isNothing)
                 |> List.map (dispDiscard st)
                 |> Util.groupInto 3 6
                 |> List.map (flow right)
                 |> flow down
                 |> container (2 * infoBlockWidth) (3 * t_h) topLeft

dispDiscard : GameState -> Discard -> Element
dispDiscard st d = if d.riichi
   then collage t_h t_w [ rotate (degrees 90) <| toForm <| dispTile st d.tile ]
   else dispTile st d.tile

-- }}}

-- {{{ Display: Hand: Others

-- | Tiles hidden in other's hands. XXX: Doesn't show the picked tile.
dispHiddenTiles : HandPublic -> Element
dispHiddenTiles h =
   let num = 13 - 2 * List.length h.called
       in flow right <| List.repeat num tileHidden

dispOthersHand : GameState -> Kaze -> HandPublic -> Element
dispOthersHand st k h = dispHiddenTiles h `beside` dispPublicMentsu st k h

-- }}}

-- {{{ Display: Mentsu ----------------------------------------------------------------

-- | The second argument tells who we are. We need to know in order to rotate the
-- correct tile in a shouted mentsu.
dispMentsu : GameState -> Kaze -> Mentsu -> Element
dispMentsu st k m = case m.from of
   Nothing -> dispMentsuConcealed st m
   Just s  -> dispShout st k m s

dispMentsuConcealed : GameState -> Mentsu -> Element
dispMentsuConcealed st m = flow right <| map (dispTile st) <| m.tiles

dispPublicMentsu : GameState -> Kaze -> HandPublic -> Element
dispPublicMentsu st k h = flow right <| map (dispMentsu st k) h.called

dispShout : GameState -> Kaze -> Mentsu -> Shout -> Element
dispShout st k m s =
   let myTiles  = map (dispTile st) s.shoutTo
       rotated  = rotate (degrees 90) <| toForm <| dispTile st s.shoutTile
       shoutPos =
          if s.shoutKind == Pon && m.mentsuKind == Kantsu -- shouminkan?
             then collage t_h (2*t_w) [moveY (t_w / 2) rotated, moveY (-t_w / 2) rotated]
             else collage t_h t_w [rotated]
       nth_from = kazeNth s.shoutFrom
       nth_me   = kazeNth k

   in flow right <|
         if (nth_from + 1) % 4 == nth_me then --left side
            shoutPos :: myTiles
         else if (nth_me + 1) % 4 == nth_from then -- right side
            List.take 2 myTiles ++ [shoutPos] ++ List.drop 2 myTiles
         else -- center
            List.take 1 myTiles ++ [shoutPos] ++ List.drop 1 myTiles

-- }}}

-- {{{ Display: Tiles --------------------------------------------------------------

dispTile : GameState -> Tile -> Element
dispTile = tileImage False

dispTileHand : GameState -> Tile -> Element
dispTileHand = tileImage True

dispWanpai : GameState -> Element
dispWanpai st = st.rs.dora |> map (dispTile st) |> flow right

tileImage : Bool -> GameState -> Tile -> Element
tileImage inhand st tile =
   let (col, row) = case tile of
               Suited ManTile n False -> ((n - 1) * t_w, 0)
               Suited ManTile n True  -> (9       * t_w, 0)

               Suited PinTile n False -> ((n - 1) * t_w, t_h)
               Suited PinTile n True  -> (9       * t_w, t_h)

               Suited SouTile n False -> ((n - 1) * t_w, 2 * t_h)
               Suited SouTile n True  -> (9       * t_w, 2 * t_h)

               Honor (Kazehai k)   -> (kazeNth k * t_w, 3 * t_h)
               Honor (Sangenpai s) -> ((4 + sangenNth s) * t_w, 3 * t_h)

       (row', t_h') = if inhand then (row, t_h) else (row + (t_h - 37), 37)
   in
      croppedImage (col, row') t_w t_h' <| lookupResource st "tiles"

tileHidden : Element
tileHidden = collage t_w (t_h // 3) [ rect t_w (t_h / 3) |> outlined defaultLine ]

-- }}}

-- {{{ Buttons -----------------------------------------------------------------

displayShoutButtons : GameState -> List Shout -> List Element
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

shouminkanButtons : RoundState -> String -> List Element
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
