module Game where

import Util exposing (..)
import Lounge
import GameTypes exposing (..)
import Model exposing (GameState, Status(..))
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
         setRs { st | gameFinalPoints = Nothing } rs

      RoundPrivateWaitForTurnAction {seconds, riichiWith} ->
         setRs st { rs | waiting = Just { player = rs.player, seconds = toFloat seconds, riichiWith = riichiWith, shouts = [] } }

      RoundPrivateWaitForShout {seconds, shouts} ->
         setRs st { rs | waiting = Just { player = rs.player, seconds = toFloat seconds, riichiWith = [], shouts = shouts } }

      RoundPrivateChange {hand} -> setRs st { rs | myhand = hand }

      RoundTurnBegins {player_kaze} ->
         setRs { st | turnBegan = st.updated }
               { rs | turn = player_kaze, waiting = Just { player = -1, seconds = -1, riichiWith = [], shouts = [] } }
              |> Util.log ("Turn of " ++ toString player_kaze)

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
   maybe [] (displayShoutButtons st) (st.rs.waiting)
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
      , kazeImage st k |> toForm |> scale 0.7 |> moveRotateKaze (2.2 * t_w, 0) st.rs.mypos k ]
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

      let dispTenpaiHand {mentsu,tiles} = flow right <| map (dispTile st) tiles ++ map (dispMentsu st k) mentsu

          dispVisible = st.rs.results `Maybe.andThen` (\res -> case res of
            DealDraw{tenpai}   -> Maybe.map dispTenpaiHand <| Util.listFindWith .player_kaze k tenpai
            DealTsumo{winners} -> Maybe.map (.valuehand >> dispValued st) <| Util.listFindWith .player_kaze k winners
            DealRon{winners}   -> Maybe.map (.valuehand >> dispValued st) <| Util.listFindWith .player_kaze k winners
             )

      in Maybe.withDefault (Util.listFind k st.rs.hands |> dispOthersHand st k) dispVisible
         |> toForm
         |> moveRotateKaze (handOffCenter, 0) st.rs.mypos k
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
      , toForm (dispWanpai st)               |> move (0, 25)
      , dealAndRoundIndicator st             |> move (30, -20)
      , honbaIndicator st.rs.honba           |> move (35, -40)
      , riichiInTableIndicator st.rs.inTable |> move (60, -40)
      , tilesLeftIndicator st.rs             |> move (40, -60)
      ]

tilesLeftIndicator : RoundState -> Form
tilesLeftIndicator rs =
   toForm <| centered <| T.fromString
   <| toString rs.tilesleft

dealAndRoundIndicator : GameState -> Form
dealAndRoundIndicator st =
   group
   [ scale 0.6 <| toForm <| kazeImage st (st.rs.round.kaze)
   , moveX 15 <| toForm <| centered (T.fromString (toString <| st.rs.round.round_rot))
   ]

turnIndicator : GameState -> Form
turnIndicator st = case st.rs.waiting of
   Nothing -> toForm empty
   Just ws ->
      let col = if List.isEmpty ws.shouts
                   then lightGreen
                   else lightOrange

          secs = if ws.seconds < 0 && ws.player == st.rs.player
                    then empty 
                    else show <| floor ws.seconds
      in group
         [ rotate (degrees 90) <| filled col <| ngon 3 80
         , moveY 40 <| toForm secs
         ]

honbaIndicator h =
   if h > 0
      then toForm <| centered <| T.fromString <| toString h ++ "H"
      else toForm empty

riichiInTableIndicator n =
   if n > 0
      then toForm <| centered <| T.color red <| T.fromString <| toString (n / 1000) ++ "R"
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

-- | like @moveRotateKaze@, but does not rotate.
moveKaze : (Float, Float) -> Kaze -> Kaze -> Form -> Form
moveKaze (offY, offX) mypos pos =
   case (kazeNth pos - kazeNth mypos) % 4 of
      0 -> moveY (-offY) << moveX offX
      1 -> moveX offY    << moveY offX
      2 -> moveY offY    << moveX (-offX)
      _ -> moveX (-offY) << moveY (-offX)

-- | Rotates only those adjacant to ourself.
moveRotateAdjacentKaze : (Float, Float) -> Kaze -> Kaze -> Form -> Form
moveRotateAdjacentKaze (offY, offX) mypos pos =
   case (kazeNth pos - kazeNth mypos) % 4 of
      0 -> moveY (-offY) << moveX offX
      1 -> moveX offY    << moveY offX << rotate (degrees 90) 
      2 -> moveY offY    << moveX (-offX)
      _ -> moveX (-offY) << moveY (-offX) << rotate (degrees 270)

-- | Riichi stick, not rotated.
playerRiichi : GameState -> Form
playerRiichi st = toForm <| image (infoBlockWidth - 20) 20 <| lookupResource st "stick-1000"
-- }}}

-- {{{ Display: Results ----------------------------------------------
dispResults : GameState -> RoundResult -> Form
dispResults st res = case res of
   DealDraw {tenpai, nooten} -> dispDraw st tenpai nooten
   DealTsumo {winners, payers} -> dispTsumo st winners payers
   DealRon {winners, payers} -> dispRon st winners payers

dispDraw : GameState -> List Tenpai -> List Payer -> Form
dispDraw st tenpai nooten = group
   [ toForm
      <| color yellow
      <| opacity 0.3
      <| container infoBlockWidth infoBlockWidth middle empty
   , toForm
      <| titleText "Draw!"
   , toForm
      <| collage playAreaWidth playAreaHeight
      <| map (dispTenpai st) tenpai ++ map (dispPayer st) nooten
   ]

dispTsumo : GameState -> List Winner -> List Payer -> Form
dispTsumo st winner payer =
   toForm
      <| collage playAreaWidth playAreaHeight
      <| map (dispWinner st <| T.fromString "Tsumo") winner
      ++ map (dispPayer st) payer

dispRon : GameState -> List Winner -> List Payer -> Form
dispRon st winner payer =
   toForm
      <| collage playAreaWidth playAreaHeight
      <| map (dispWinner st <| T.fromString "Ron") winner
      ++ map (dispPayer st) payer

-- | Displays the points rotated to correct place
dispTenpai : GameState -> Tenpai -> Form
dispTenpai st {player_kaze, points} =
   dispPoints points
   |> centered
   |> toForm
   |> moveKaze (220,0) st.rs.mypos player_kaze

-- | Displays the points rotated to correct place
dispPayer : GameState -> Payer -> Form
dispPayer st {player_kaze, points} =
   dispPoints points
   |> centered
   |> toForm
   |> moveKaze (220,0) st.rs.mypos player_kaze

{-| points and yaku oriented towards the winner -}
dispWinner : GameState -> T.Text -> Winner -> Form
dispWinner st extraT {player_kaze, points, valuehand} =
   let pointsT = dispPoints points
       handValueT = dispHandValue valuehand.value
   in T.concat
         [ pointsT
         , T.fromString "\n"
         , handValueT
         , T.fromString "\n"
         , extraT
         ]
         |> centered
         |> toForm
         |> moveRotateAdjacentKaze (200,0) st.rs.mypos player_kaze

dispValued : GameState -> Valued -> Element
dispValued st {mentsu, tiles, value} =
   flow right (map (dispTileHand st) tiles ++ map (dispMentsu st Ton) mentsu) -- TODO get correct player kaze

dispHandValue : HandValue -> T.Text
dispHandValue {yaku, fu, han, points, named} =
   let firstline = case named of
            Just name -> T.fromString name |> T.color red
            Nothing   -> T.concat
               [ T.fromString (toString han), T.fromString " Han "
               , T.fromString (toString fu), T.fromString (" Fu ") ]
   in T.concat
         [ firstline
         , T.fromString "\n", T.concat (map dispYaku yaku) ]

dispPoints : Points -> T.Text
dispPoints n = if n < 0
   then T.fromString "-" `T.append` T.fromString (toString n) |> T.color red
   else T.fromString "+" `T.append` T.fromString (toString n) |> T.color green

dispYaku : Yaku -> T.Text
dispYaku {han, name} = T.concat
   [ T.fromString name |> T.bold
   , T.fromString " "
   , T.fromString ("(" ++ toString han ++ ")") |> T.color red
   , T.fromString " "
   ]
-- }}}

-- {{{ Display: My hand ---------------------------------------------------

dispHand : Kaze -> GameState -> Hand -> Element
dispHand k st hand = flow right <|
   List.map2 (dispTileInHand st) [1..14] (sortTiles hand.concealed)
   ++ [ spacer 10 10 ]
   ++ map (.tile >> maybe empty (dispTileInHand st (List.length hand.concealed + 1))) hand.picks
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
dispWanpai st =
   let dispDora = st.rs.dora |> map (dispTile st) |> flow right
       dispUra  = st.rs.uradora |> map (dispTile st) |> flow right
   in dispDora `above` dispUra

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

displayShoutButtons : GameState -> Waiting -> List Element
displayShoutButtons st {shouts} =
   let shoutButtons = map (\s -> shoutButton s
         |> hoverable (\f -> message shoutHover.address <| if f then (getTileIndices (sortTiles st.rs.myhand.concealed) s.shoutTo, Just s)
                                                                else ([], Nothing))) shouts
       nocareButton = clickable (message nocare.address True) <| buttonElem' "Pass" green
   in if List.isEmpty shouts
         then []
         else shoutButtons ++ [ nocareButton ]

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

buttonElem' str col = collage 80 40
   [ oval 80 40 |> filled col
   , T.fromString str |> centered |> toForm ]

-- }}}

-- {{{ Utility -----------------------------------------------------------------
findFourTiles : RoundState -> Maybe Tile
findFourTiles rs = counted 4 <| sortTiles <| rs.myhand.concealed ++ map (.tile >> fromJust) rs.myhand.picks

findShouminkan : Hand -> List Tile
findShouminkan h = List.filter
   (\mentsu -> mentsu.mentsuKind == Koutsu && List.any (\t -> t `List.member` mentsu.tiles) (h.concealed ++ map (.tile >> fromJust) h.picks))
   h.called -- TODO This fails when picked more than one tile
     |> map (fromJust << List.head << .tiles)

-- | Finds a tile we have 4 of in hand
-- TODO: This only finds the first quad. Should find all of them and return a
-- list.
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
