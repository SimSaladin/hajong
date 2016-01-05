module View where

import Lounge
import MsgDialog
import Game
import Events
import Util exposing (..)
import GameTypes exposing (..)
import JSON exposing (decodeEvent, encodeEvent, encodeRoundState)

import Set
import Mouse
import Text
import Time
import Dict
import Dict exposing (Dict)
import Graphics.Input.Field as Field
import Task exposing (Task)
import Json.Decode exposing (Value)
import Graphics.Input.Field as Field
import Graphics.Element exposing (..)
import Color exposing (..)
import Signal exposing (..)
import Window
import Debug

defaultResources : Dict String String
defaultResources = Dict.fromList
   [ ("Ton", "/static/img/Ton.gif")
   , ("Nan", "/static/img/Nan.gif")
   , ("Shaa", "/static/img/Shaa.gif")
   , ("Pei", "/static/img/Pei.gif")
   , ("stick-1000", "/static/img/point1000.svg")
   , ("tiles", "/static/img/pai.png")
   ]

-- {{{ Default state ------------------------------------------
newState : GameState
newState = { status     = InLounge
           , mynick     = ""
           , myid       = 0
           , lounge     = defaultLounge
           , gameWait   = Nothing
           , supportURL = "/support/"
           , dialogFieldContent = Field.noContent

           , gameUUID   = Nothing
           , waitTurnAction = Nothing
           , gameFinalPoints = Nothing
           , waitShout  = Nothing
           , turnBegan  = 0
           , riichiWith = []
           , hoveredTileNth = -1
           , relatedToShout = []
           , rs = emptyRoundState

           , lobbyChosenGame = Nothing
           , profilePictures = Dict.empty

           , resources = defaultResources

           , updated    = 0
           , dimensions = (500, 500) -- Needs to be refreshed

           , logging    = []
           }

emptyRoundState =
   { mypos         = Ton
   , round         = { kaze = Ton, round_rot = 0, round_honba = 0 }
   , turn          = Ton
   , player        = -1
   , oja           = -1
   , firstoja      = -1
   , tilesleft     = -1
   , dora          = []
   , hands         = []
   , players       = []
   , myhand        = { concealed = [], picks = [], furiten = NotFuriten,
                        canTsumo = False, state = DrawNone, called = [],
                        discards = [], riichiState = NoRiichi, ippatsu = True }
   , results       = Nothing
   , honba         = 0
   , inTable       = 0
   , eventsHistory = []
   }

-- }}}

type Input = AnEvent Event
           | GameInput Game.UserInput
           | LoungeInput Lounge.UserInput
           | MsgDialogInput MsgDialog.UserInput 
           | Dimensions (Int, Int)
           | TimeDelta Time.Time
           | ReceivedProfilePictures ProfilePictures 

minDimensions : (Int, Int) -> (Int, Int)
minDimensions (x, y) = (max x 260, max y 260)

stepGame : Input -> GameState -> GameState
stepGame x gs = case x of
   AnEvent event      -> stepEvent event <| { gs | logging =
      maybe gs.logging (\x -> x :: gs.logging) <| MsgDialog.eventToDebugMsg event }

   GameInput inp      -> Game.stepUserInput inp gs
   LoungeInput inp    -> Lounge.stepUserInput inp gs
   MsgDialogInput inp -> MsgDialog.stepUserInput inp gs
   Dimensions x       -> { gs | dimensions = x }
   ReceivedProfilePictures pics -> { gs | profilePictures = gs.profilePictures `Dict.union` pics }
   TimeDelta time     -> { gs | updated   = time
                              , turnBegan = if gs.turnBegan == 0 then time else gs.turnBegan
                         }

-- | Apply an Event to the GameState.
stepEvent : Event -> GameState -> GameState
stepEvent event gameState = case event of
   Identity   {nick}   -> { gameState | mynick = nick }
   JoinServer {nick}   -> { gameState | lounge = addIdle nick gameState.lounge }
   PartServer {nick}   -> { gameState | lounge = deleteNick nick gameState.lounge }
   Message {from,content} -> gameState -- TODO
   Invalid {content} -> gameState -- TODO
   LoungeInfo {lounge} -> { gameState | lounge = lounge }
   GameCreated {game}  -> { gameState | lounge = addGame game gameState.lounge }
   JoinGame {nick, ident} ->
       { gameState | lounge   = addJoinedGame ident nick gameState.lounge
                   , gameWait = if gameState.mynick == nick then Just ident
                                                            else gameState.gameWait
                   , gameUUID = Maybe.map .uuid <| lookupGameInfo gameState ident
       }
   InGameEvents events ->
      List.foldl Game.processInGameEvent gameState events
   _ -> gameState

-- {{{ Nick and game fiddling ---------------------------------
addGame g l = { l | games = l.games ++ [g] }
addIdle n l = { l | idle  = Set.insert n l.idle }

addJoinedGame i n l =
    { l | games = List.map (\g -> if g.ident == i then { g | players = Set.insert n g.players } else g) l.games
        , idle  = Set.remove n l.idle }

deleteNick : String -> LoungeData -> LoungeData
deleteNick n l =
   { l | idle  = Set.remove n l.idle
       , games = List.map (\g -> { g | players = Set.remove n g.players }) l.games }
-- }}}

-- {{{ Display ------------------------------------------------

display : GameState -> Element
display st = case st.status of
   InLounge -> flow down [ Lounge.display st, MsgDialog.dialog st ]
   InGame rs -> flow down [ Game.display st ]

-- }}}
