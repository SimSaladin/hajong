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

------------------------------------------------------------------------------

-- * Points, results

data RoundResults = RoundTsumo { winners :: [Player], payers :: [Player] }
                  | RoundRon   { winners :: [Player], payers :: [Player] }
                  | RoundDraw  { winners :: [Player], payers :: [Player] }
                  deriving (Show, Read)

type Points = Int

type RiichiPlayers playerID = [(Player, playerID, Points)]

-- * Round

-- | This would be better renamed as RoundState, as that's what it really is.
data RiichiState = RiichiState
                 { _riichiSecret :: RiichiSecret
                 , _riichiPublic :: RiichiPublic
                 , _riichiEvents :: [GameEvent]
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
                 , _riichiPlayers :: RiichiPlayers Text
                 , _riichiResults :: Maybe RoundResults
                 } deriving (Show, Read)

-- | State of single player. Note that there is no RiichiSecret.
data GamePlayer = GamePlayer
                { _playerPlayer :: Player -- ^ Me
                , _playerPublic :: RiichiPublic
                , _playerPublicHands :: Map Player HandPublic
                , _playerPlayers :: RiichiPlayers Text
                , _playerMyHand :: Hand
                } deriving (Show, Read)

-- * Actions and events

data GameEvent = RoundPrivateStarts GamePlayer -- ^ Only at the start of a round
               | RoundPrivateWaitForShout Int -- ^ Number of seconds left to shout or confirm an ignore (See @GameDontCare@)
               | RoundPrivateChange Player Hand
               | RoundTurnBegins Player
               | RoundTurnAction Player TurnAction
               | RoundTurnShouted Player Shout -- ^ Who, Shout
               | RoundHandChanged Player HandPublic
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

defaultPlayers :: [Player]
defaultPlayers = [Player Ton .. Player Pei]

-- | Four-player riichi game
newPublic :: RiichiPublic
newPublic = RiichiPublic
    { _riichiDora          = []
    , _riichiWallTilesLeft = 0
    , _riichiRound         = Ton
    , _riichiDealer        = Player Ton
    , _riichiTurn          = Player Ton
    , _riichiPlayers       = zip3 defaultPlayers (repeat "") (repeat 25000)
    , _riichiResults       = Nothing
    }

newSecret :: IO RiichiSecret
newSecret = liftM dealTiles $ shuffleM riichiTiles
    where
        dealTiles tiles = RiichiSecret
            { _riichiWall           = wall
            , _riichiWanpai         = wanpai
            , _riichiHands          = Map.fromList $ zip defaultPlayers (map initHand [h1, h2, h3, h4])
            , _riichiWaitShoutsFrom = []
            } where
                (hands, xs)             = splitAt (13 * 4) tiles
                ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
                (wanpai, wall)          = splitAt 14 xs

-- | New state with first round ready to start. Convenient composite of
-- newPublic, newSecret and setSecret.
newRiichiState :: IO RiichiState
newRiichiState = liftM (`setSecret` newPublic) newSecret

setSecret :: RiichiSecret -> RiichiPublic -> RiichiState
setSecret secret public = RiichiState
    (secret & set riichiWanpai wanpai')
    (public & set riichiDora [dora] & set riichiWallTilesLeft (secret ^. riichiWall.to length))
    []
    where
        (dora : wanpai') = secret ^. riichiWanpai

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound rs = do
    secret <- newSecret
    return $ setSecret secret $ (rs ^. riichiPublic) & set riichiTurn (rs ^. riichiPublic.riichiDealer)
