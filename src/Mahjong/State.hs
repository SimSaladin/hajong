------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.State
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.State where

------------------------------------------------------------------------------
import           Mahjong.Tiles
import           Mahjong.Hand
import           Mahjong.Hand.Mentsu

------------------------------------------------------------------------------
import qualified Data.Map as Map
import           System.Random.Shuffle (shuffleM)
import           System.Random (randomRIO)

------------------------------------------------------------------------------

-- * Players

-- | 0..3
newtype Player = Player Int
                 deriving (Show, Read, Eq, Ord)

-- * Points, results

data RoundResults = RoundTsumo { winners :: [Player], payers :: [Player] }
                  | RoundRon   { winners :: [Player], payers :: [Player] }
                  | RoundDraw  { winners :: [Player], payers :: [Player] }
                  deriving (Show, Read)

type Points = Int

type RiichiPlayers player = Map Kaze (Player, player, Points)

-- * Round

-- | Game state, including current round state.
data RiichiState = RiichiState
                 { _riichiRounds :: [(Kaze, Int)] -- ^ Decreasing in play order, current first
                 , _riichiSecret :: RiichiSecret
                 , _riichiPublic :: RiichiPublic
                 , _riichiEvents :: [GameEvent]
                 } deriving (Show, Read)

-- | Round state, secret half.
data RiichiSecret = RiichiSecret
                 { _riichiWall :: [Tile]
                 , _riichiWanpai :: [Tile]
                 , _riichiHands :: Map Kaze Hand
                 , _riichiWaitShoutsFrom :: [Kaze]
                 } deriving (Show, Read)

-- | Round state, public half.
data RiichiPublic = RiichiPublic
                 { _riichiDora :: [Tile]
                 , _riichiWallTilesLeft :: Int
                 , _riichiRound :: Kaze
                 , _riichiTurn :: Kaze
                 , _riichiOja :: Player
                 , _riichiFirstOja :: Player
                 , _riichiPlayers :: [(Kaze, Player, Points)]
                 , _riichiResults :: Maybe RoundResults
                 } deriving (Show, Read)

-- | State of single player. Note that there is no RiichiSecret.
data GamePlayer = GamePlayer
                { _playerKaze :: Kaze
                , _playerPlayer :: Player -- ^ Me
                , _playerPublic :: RiichiPublic
                , _playerPublicHands :: Map Kaze HandPublic
                , _playerMyHand :: Hand
                } deriving (Show, Read)

-- * Actions and events

data GameEvent = RoundPrivateStarts GamePlayer -- ^ Only at the start of a round
               | RoundPrivateWaitForShout Player Int -- ^ Number of seconds left to shout or confirm an ignore (See @GameDontCare@)
               | RoundPrivateWaitForTurnAction Player Int
               | RoundPrivateChange Player Hand
               | RoundTurnBegins Kaze
               | RoundTurnAction Kaze TurnAction
               | RoundTurnShouted Kaze Shout -- ^ Who, Shout
               | RoundHandChanged Kaze HandPublic
                        -- TODO this is a bit too vague to be exactly
                        -- useful for clients.. perhaps could identify
                        -- between draws, kans, shouts.
               | RoundEnded RoundResults
               deriving (Show, Read)

-- | Actions you do on your turn.
data TurnAction = TurnTileDiscard Bool Tile -- ^ Riichi?
                | TurnTileDraw Bool (Maybe Tile) -- ^ wanpai?, tile
                | TurnAnkan Tile
                deriving (Show, Read)

-- | Actions you do on someone else's turn.
data GameAction = GameTurn TurnAction
                | GameShout Shout
                | GameDontCare -- ^ About shouting last discarded tile
                deriving (Show, Read)

-- * Lenses

--
makeLenses ''RiichiSecret
makeLenses ''RiichiPublic
makeLenses ''RiichiState
makeLenses ''GamePlayer

-- * Initialize state

-- | New state with first round ready to start. Convenient composite of
-- newPublic, newSecret and setSecret.
newRiichiState :: IO RiichiState
newRiichiState = RiichiState
    <$> pure [(Ton, 0)]
    <*> newSecret
    <*> (newPublic fourPlayers . Player <$> randomRIO (0, 3))
    <*> pure []

fourPlayers :: [Player]
fourPlayers = Player <$> [0 .. 3]

-- | Four-player riichi game
newPublic :: [Player] -- ^ Players
          -> Player   -- ^ Oja
          -> RiichiPublic
newPublic players oja = RiichiPublic
    { _riichiDora          = []
    , _riichiWallTilesLeft = 0
    , _riichiRound         = Ton
    , _riichiTurn          = Ton
    , _riichiOja           = oja
    , _riichiFirstOja      = oja
    , _riichiPlayers       = zip3 [Ton .. Pei] players (repeat 25000)
    , _riichiResults       = Nothing
    }

newSecret :: IO RiichiSecret
newSecret = liftM dealTiles $ shuffleM riichiTiles
    where
        dealTiles tiles = RiichiSecret
            { _riichiWall           = wall
            , _riichiWanpai         = wanpai
            , _riichiHands          = Map.fromList $ zip [Ton .. Pei] (initHand <$> [h1, h2, h3, h4])
            , _riichiWaitShoutsFrom = []
            } where
                (hands, xs)             = splitAt (13 * 4) tiles
                ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
                (wanpai, wall)          = splitAt 14 xs

setSecret :: RiichiSecret -> RiichiState -> RiichiState
setSecret secret =
    (riichiSecret .~ s secret) .
    (riichiPublic %~ p) .
    (riichiEvents .~ [])
  where
    (dora : wanpai') = secret ^. riichiWanpai
    s = riichiWanpai .~ wanpai'
    p = (riichiDora .~ [dora]) .
        (riichiWallTilesLeft .~ (secret ^. riichiWall.to length))
