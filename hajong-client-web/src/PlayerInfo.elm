module PlayerInfo where

import Http

import Json.Decode exposing (Decoder, (:=))
import Json.Decode as Json

import Task exposing (..)

import Dict
import Dict exposing (Dict)
import Set
import Set exposing (Set)

-- | A mapping from usernames to user information.
type alias InfoDict = Dict String PlayerInfo

type alias PlayerInfo =
   { displayName    : String
   , profilePicture : String -- ^ An URL
   }

type alias ST a = { a | playerInfo : InfoDict }

-- XXX: The genericity in this function is somehow toxic and blows the elm
-- compiler.. The specialized version is in Lounge.
--
-- getInfoWithDefault : ST a -> a -> (PlayerInfo -> a) -> String -> a
-- getInfoWithDefault st def f k = Dict.get k st.playerInfo
--    |> Maybe.map f
--    |> Maybe.withDefault def

updates : Signal.Mailbox (ST a -> ST a)
updates = Signal.mailbox identity

-- | HTTP Requests signal
requests : Signal (Set String) -- ^ All nicks to fetch player info for.
        -> Signal (Task Http.Error ())
requests nickSignal =

   let f nicks (fetched, _) = (Set.union fetched nicks, Set.toList <| Set.diff nicks fetched)
               -- accumulator: (already-fetched, to-fetch)

      in Signal.foldp f (Set.empty, []) nickSignal
         |> Signal.dropRepeats
         |> Signal.map (\(_, new) ->
            lookupInfoDict new
            `andThen` (\info -> Signal.send updates.address (updateDict info)))

updateDict : InfoDict -> ST a -> ST a
updateDict dict st = { st | playerInfo = dict `Dict.union` st.playerInfo }

-- {{{ tasks

-- | Based on usernames
lookupInfoDict : List String -> Task Http.Error InfoDict
lookupInfoDict usernames =
   Http.get decodeInfoDict <| Http.url "/api/player-info" <| List.map ((,) "u") usernames

-- }}}

-- {{{ decoders

decodeInfoDict : Decoder InfoDict 
decodeInfoDict = Json.dict decodePlayerInfo

decodePlayerInfo : Decoder PlayerInfo
decodePlayerInfo = Json.object2 PlayerInfo
   ("name"    := Json.string)
   ("picture" := Json.string)

-- }}}
