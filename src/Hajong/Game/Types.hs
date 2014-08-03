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
                   } deriving (Show, Read)


-- * Round

-- | This would be better renamed as RoundState, as that's what it really is.
data RiichiState = RiichiState
                 { _riichiSecret :: RiichiSecret
                 , _riichiPublic :: RiichiPublic
                 } deriving (Show, Read)

data RiichiSecret = RiichiSecret
                 { _riichiWall :: [Tile]
                 , _riichiWanpai :: [Tile]
                 , _riichiHands :: Map Player Hand
                 , _riichiWaitShoutsFrom :: [Player]
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

data TurnAction = TurnTileDiscard Bool Tile -- ^ Riichi?
                | TurnTileDraw Bool (Maybe Tile) -- ^ From wanpai? - sensitive!
                | TurnAnkan Tile
                | TurnShouted Shout Player -- ^ tile, shout, shouter
                | TurnAuto -- ^ Special never-failing and definitely turn-ending action
                deriving (Show, Read)

data RoundEvent = RoundTurnBegins Player
                | RoundTurnAction Player TurnAction
                | RoundPublicHand Player HandPublic
                | RoundTsumo Player
                | RoundRon Player [Player] -- From, who?
                | RoundDraw [Player] -- tenpai players
                deriving (Show, Read)

data RoundResults = RoundResults

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
--
-- Note that RiichiSecret is freely modifiable as a state, but
-- @RiichiPublic@ is read-only. Modifications to the public part must
-- be encoded in RoundEvents. This way it is trivial to keep clients'
-- public states in sync with minimum bandwidth.
type RoundM m = ( MonadReader RiichiPublic m
                , MonadState RiichiSecret m
                , MonadWriter [RoundEvent] m
                , MonadError Text m
                , Functor m
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
