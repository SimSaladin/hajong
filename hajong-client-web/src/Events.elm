module Events where

{-| Wrappers around the `Event`-type. -}

import GameTypes exposing (..)

-- Create Events ------------------------------------------------------------

joinGame : Int -> Event
joinGame n = JoinGame { nick = "", ident = n }

forceStart : Int -> Event
forceStart n = ForceStart { ident = n }

