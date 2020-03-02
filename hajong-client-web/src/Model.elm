module Model where

import PlayerInfo
import GameTypes exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)
import Graphics.Input.Field as Field

type alias GameState =
   { status     : Status
   , mynick     : String
   , myid       : Int
   , gameWait   : Maybe Int
   , supportURL : String
   , dialogFieldContent : Field.Content

   -- lounge-related
   , lounge          : LoungeData
   , lobbyChosenGame : Maybe GameInfo
   , playerInfo      : PlayerInfo.InfoDict

    -- In-Game
   , gameFinalPoints : Maybe FinalPoints
   , turnBegan      : Time
   , riichiWith     : List Tile
   , gameUUID       : Maybe String
   , hoveredTileNth : Int
   , relatedToShout : List Int
   , rs             : RoundState

   , resources : Dict String String -- image urls

   -- properties
   , updated    : Time
   , dimensions : (Int, Int)
   -- Debug
   , logging    : List LogItem
   }

{-| This tag defines the active -}
type Status = InLounge
            | InGame RoundState

type LogItem = LogMsg { player_nick : String, msg : String }
             | LogDebug { msg : String }
             | LogError { msg : String }

type alias ProfilePictures = Dict String String
