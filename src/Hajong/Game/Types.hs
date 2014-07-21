------------------------------------------------------------------------------
-- | 
-- Module         : GameTypes
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.Types where

import ClassyPrelude
import Control.Lens

-- * Utility

if' :: Bool -> t -> t -> t
if' cond th el = if cond then th else el

-- * Game state

-- | Server-side state
data GameServer playerID = GameServer
                   { _gamePlayers :: RiichiPlayers playerID
                   , _gameName :: Text
                   , _gameState :: Maybe RiichiState -- maybe in running game
                   }

type RiichiPlayers playerID = [(Player, Maybe playerID, Points)]

type Points = Int

-- * Round

data RiichiState = RiichiState
                 { _riichiSecret :: RiichiSecret
                 , _riichiPublic :: RiichiPublic
                 }

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
                 , _riichiEvents :: [(Player, TurnAction, Int)]
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

-- * Hands

data HandPublic = HandPublic
                { _handOpen :: [Mentsu]
                , _handDiscards :: [(Tile, Maybe Player)]
                , _handRiichi :: Bool
                , _handTurnDiscard :: Maybe (Tile, UTCTime)
                } deriving (Show, Read, Eq)

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handPick :: Maybe Tile
          , _handFuriten :: Maybe Bool -- ^ Just (temporary?)
          , _handPublic :: HandPublic
          } deriving (Show, Read, Eq)

-- * Players

-- | State of single player
data GamePlayer playerID = GamePlayer
                  { _playerPlayer :: Player
                  , _playerPublic :: RiichiPublic
                  , _playerPublicHands :: Map Player HandPublic
                  , _playerPlayers :: RiichiPlayers playerID
                  , _playerMyHand :: Hand
                  } deriving (Show, Read)

-- * Tiles

newtype Player = Player Kazehai deriving (Show, Read, Eq, Ord)
deriving instance Enum Player

data Number = Ii | Ryan | San | Suu | Wu | Rou | Chii | Paa | Chuu
            deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Sangenpai = Haku | Hatsu | Chun
               deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Kazehai = Ton | Nan | Shaa | Pei
             deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Tile = Man Number Bool
          | Pin Number Bool
          | Sou Number Bool
          | Sangen Sangenpai
          | Kaze Kazehai
          deriving (Show, Read, Eq, Ord)

-- * Mentsu

data Mentsu = Shuntsu { mentsuPai :: [Tile], mentsuOpen :: Maybe Player } -- straight
            | Koutsu  { mentsuPai :: [Tile], mentsuOpen :: Maybe Player } -- triplet
            | Kantsu  { mentsuPai :: [Tile], mentsuOpen :: Maybe Player } -- quadret
            | Jantou  { mentsuPai :: [Tile], mentsuOpen :: Maybe Player } -- pair
            deriving (Show, Read, Eq, Ord)

-- * Shouts

data Shout = Pon
           | Kan
           | Chi (Tile, Tile)
           | Ron
           deriving (Show, Read, Eq)

-- * Lenses

makeLenses ''GameServer
makeLenses ''GamePlayer
makeLenses ''RiichiSecret
makeLenses ''RiichiPublic
makeLenses ''RiichiState
makeLenses ''HandPublic
makeLenses ''Hand

handOf :: Player -> Lens RiichiSecret RiichiSecret (Maybe Hand) (Maybe Hand)
handOf player = riichiHands.at player
