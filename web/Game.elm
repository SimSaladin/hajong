module Game where

import State (..)

display : {} -> GameState -> Element
display _ gs = doDraw gs 

doDraw : GameState -> Element
doDraw gs = asText "In a game"

view : Signal {}
view = constant {}
