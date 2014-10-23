module Game where

import Util
import GameTypes (..)

type Controls = {}

display : Controls -> GameState -> Element
display co gs = case gs.roundState of
   Just rs -> flow down
      [ asText "In a game"
      , asText <| "I am a..." ++ show rs.mypos
      , asText <| "Round is " ++ show rs.round
      , asText <| "dealer is " ++ show rs.dealer
      , asText <| "My hand: " ++ show rs.myhand
      , asText <| "It is no turn of " ++ show rs.turn
      , asText <| "Dora is/are " ++ show rs.dora
      , asText <| "tiles left: " ++ show rs.tilesleft
      , asText <| "Other's hands: " ++ show rs.hands
      , asText <| "players: " ++ show rs.players
      , asText <| "Results: " ++ show rs.results
      , asText <| "action log: " ++ show rs.actions
      ]
   Nothing -> asText "Hmm, roundState is Nothing"

view : Signal Controls
view = constant {}

processInGameEvent : GameEvent -> GameState -> GameState
processInGameEvent event gs = case event of
   RoundPrivateStarts rs ->
     if gs.status == InGame
        then Util.log gs "ERROR: InGame and new round!" -- TODO
        else { gs | status <- InGame
                  , gameWait <- Nothing
                  , roundState <- Just rs }

   RoundPrivateWaitForShout {seconds} ->
      Util.log gs ("Waiting for shouts... " ++ show seconds)
 
   RoundPrivateChange {player, hand} ->
      setMyHand hand gs
 
   RoundTurnBegins {player} ->
      Util.log gs ("Turn of " ++ show player) |> setTurnPlayer player
 
   RoundTurnAction {player, action} ->
      addTurnAction player action gs |> processTurnAction

   RoundTurnShouted {player, shout} ->
      Util.log gs <| show player ++ " shouted: " ++ show shout -- TODO prettify

   RoundHandChanged {player, hand} ->
      setPlayerHand player hand gs

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
processTurnAction gs = gs -- TODO Implement me

updateHand player hand hands = case hands of
   (p, hand') :: xs -> if p == player
                          then (p, hand) :: xs
                          else (p, hand') :: updateHand player hand xs
