------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Types
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.Types where

import ClassyPrelude
import Control.Monad.Error (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.State (MonadState)
import Control.Monad.RWS
import Control.Lens

-- * Game state

type RiichiPlayers playerID = [(Player, Maybe playerID, Points)]

type Points = Int

-- * Game

-- | Server-side state
data GameState playerID = GameState
                   { _gamePlayers :: RiichiPlayers playerID
                   , _gameName :: Text
                   , _gameRound :: Maybe RiichiState -- maybe in running game
                   }

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

-- * Context

-- | Context of game and deal flow.
type RoundM m = ( MonadReader RiichiPublic m
                , MonadState RiichiSecret m
                , MonadWriter [RoundEvent] m
                , MonadError Text m
                )

type RoundM' = RWST RiichiPublic [RoundEvent] RiichiSecret (Either Text)

-- * Player wrappers

-- | State of single player. Note that there is no RiichiSecret.
data GamePlayer playerID = GamePlayer
                  { _playerPlayer :: Player -- ^ Me
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

-- |
makeLenses ''GameState
makeLenses ''GamePlayer
makeLenses ''RiichiSecret
makeLenses ''RiichiPublic
makeLenses ''RiichiState
makeLenses ''HandPublic
makeLenses ''Hand

handOf :: Player -> Lens RiichiSecret RiichiSecret (Maybe Hand) (Maybe Hand)
handOf player = riichiHands.at player

-- * Utility

if' :: Bool -> t -> t -> t
if' cond th el = if cond then th else el

liftE :: RoundM m => Either Text a -> m a
liftE = either throwError return

handOf' :: RoundM m => Player -> m Hand
handOf' player = use (handOf player) >>= maybe (throwError "Player not found") return
