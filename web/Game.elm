module Game where

import Util
import GameTypes (..)

display : {} -> GameState -> Element
display _ gs = doDraw gs 

doDraw : GameState -> Element
doDraw gs = asText "In a game"

view : Signal {}
view = constant {}

processInGameEvent : GameEvent -> GameState -> GameState
processInGameEvent event gs = case event of
   RoundPrivateStarts rs -> gs
   RoundPrivateWaitForShout {seconds} -> Util.log gs (show seconds)
