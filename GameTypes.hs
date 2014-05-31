------------------------------------------------------------------------------
-- | 
-- Module         : GameTypes
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GameTypes
    ( module Hand
    , module Tiles
    , module GameTypes

    -- * Re-exports
    , module Control.Lens
    ) where

import ClassyPrelude
import Control.Lens
import Tiles
import Hand

-- | Server-side state
data GameServer playerID = GameServer
                   { _gamePlayers :: RiichiPlayers playerID
                   , _gameName :: Text
                   , _gameState :: Maybe RiichiState -- maybe in running game
                   }

type RiichiPlayers playerID = [(Player, Maybe playerID, Points)]

type Points = Int

-- * Single riichi deal types

type RiichiState = (RiichiSecret, RiichiPublic)

data RiichiSecret = RiichiSecret
                 { _riichiWall :: [Tile]
                 , _riichiWanpai :: [Tile]
                 , _riichiHands :: Map Player Hand
                 } deriving (Show, Read)

data RiichiPublic = RiichiPublic
                 { _riichiDora :: [Tile]
                 , _riichiWallTilesLeft :: Int
                 , _riichiRound :: Kazehai
                 , _riichiDealer :: Player
                 , _riichiTurn :: Player
                 , _riichiPoints :: Map Player Points
                 , _riichiEvents :: [Either Shout TurnAction]
                 } deriving (Show, Read)

data TurnAction = TurnRiichi Tile
                | TurnDiscard Tile
                | TurnDraw Bool (Maybe Tile)
                | TurnAnkan Tile
                | TurnShouted Shout Player -- shout [by who]
                deriving (Show, Read)

data RoundEvent = RoundAction Player TurnAction
                | RoundPublicHand Player HandPublic
                | RoundTsumo Player
                | RoundRon Player [Player] -- From, who?
                | RoundDraw [Player] -- tenpai players
                deriving (Show, Read)

-- * Player-specific

-- | State of single player
data GamePlayer playerID = GamePlayer
                  { _playerPlayer :: Player
                  , _playerPublic :: RiichiPublic
                  , _playerPublicHands :: Map Player HandPublic
                  , _playerPlayers :: RiichiPlayers playerID
                  , _playerMyHand :: Hand
                  } deriving (Show, Read)

-- * Lenses

makeLenses ''GameServer
makeLenses ''GamePlayer
makeLenses ''RiichiSecret
makeLenses ''RiichiPublic

handOf :: Player -> Lens RiichiSecret RiichiSecret (Maybe Hand) (Maybe Hand)
handOf player = riichiHands.at player

-- * Utility

if' :: Bool -> t -> t -> t
if' cond th el = if cond then th else el
