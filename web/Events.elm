module Events where

import GameTypes (..)

-- Create Events ------------------------------------------------------------

joinServer : String -> Event
joinServer nick = JoinServer {nick = nick}

createGame : String -> Event
createGame topic = CreateGame topic

joinGame : Int -> Event
joinGame n = JoinGame { nick = "", ident = n }

forceStart : Int -> Event
forceStart n = ForceStart { ident = n }

