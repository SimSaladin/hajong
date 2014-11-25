{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.State
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
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
import qualified Data.List as L
import           System.Random.Shuffle (shuffleM)
import           System.Random (randomRIO)
import qualified Text.PrettyPrint.ANSI.Leijen as P

------------------------------------------------------------------------------

-- * Players

-- | Numerical identifier for players (@[0..3]@). Note that we use 'Kaze'
-- to specifify players in-game (so the logic is simpler), and use `Player'
-- in more general settings (obviously because players change positions
-- between hands).
newtype Player = Player Int deriving (Show, Read, Eq, Ord)

-- * Points, results

data DealResults = DealTsumo { dWinners :: [Winner], dPayers :: [Payer] }
                 | DealRon   { dWinners :: [Winner], dPayers :: [Payer] }
                 | DealDraw  { dTenpais :: [Player], dNooten :: [Payer] }
                 | DealAbort { dReason :: AbortiveDraw }
                  deriving (Show, Read, Typeable)

data AbortiveDraw = Unrelated9
                  | SuufontsuRenta -- ^ All four winds
                  | SuuchaRiichi -- ^ All players riichi
                  | SuukanSanra -- ^ Fourth kon declared (or fifth if one player declared all four)
                  | Sanchahou -- ^ Three players ron
                  deriving (Show, Read, Typeable)

type Winner = (Player, ValuedHand)
type Payer  = (Player, Points)

-- * Deal

-- | Game state, including current round state.
--
-- Fields starting @_p@ are for public consumption and @_s@ for internal
-- only.
data Deal = Deal
    -- always public
    { _pRound         :: Kaze
    , _pDeal          :: Int
    , _pTurn          :: Kaze
    , _pOja           :: Player
    , _pFirstOja      :: Player
    , _pWallTilesLeft :: Int
    , _pDora          :: [Tile]
    , _pPlayers       :: Map Player (Kaze, Points, Text)
    , _pHonba         :: Int
    , _pRiichi        :: Int -- ^ Points in table for riichi
    , _pResults       :: Maybe DealResults
    , _pDeals         :: [(Kaze, Int)] -- ^ Previous deals in decreasing order by time

    -- secret
    , _sEvents        :: [GameEvent]
    , _sHands         :: Map Kaze Hand
    , _sWall          :: [Tile]
    , _sWanpai        :: [Tile]
    , _sWaiting       :: Maybe Waiting -- ^ Waiting turn action or shout(s)
    } deriving (Show, Read, Typeable)

-- | Deal from a player's perspective
type AsPlayer = Deal 

-- | Left for turn, right for shout(s)
type Waiting = Either WaitTurnAction [WaitShout]

type WaitShout = (Player, Kaze, Int, [Shout])
type WaitTurnAction = (Player, Kaze, Int, [Tile])

-- Pretty instances 

instance P.Pretty Deal where
    pretty Deal{..} = P.pretty _pDeals P.<$$>
        P.string "wall:"   P.<+> P.hang 0 (prettyList' _sWall) P.<$$>
        P.string "wanpai:" P.<+> P.hang 0 (prettyList' _sWanpai) P.<$$>
        P.string "hands:"  P.<+> P.hang 0 (P.list $ toList $ fmap P.pretty _sHands)

-- * Actions and events

data GameEvent = DealStarts Player Kaze AsPlayer -- ^ Only at the start of a round
               | DealWaitForShout WaitShout -- ^ Number of seconds left to shout or confirm an ignore (See @GameDontCare@)
               | DealWaitForTurnAction WaitTurnAction
               | DealTurnBegins Kaze
               | DealTurnAction Kaze TurnAction
               | DealTurnShouted Kaze Shout -- ^ Who, Shout
               | DealPublicHandChanged Kaze HandPublic
               | DealPrivateHandChanged Player Kaze Hand -- ^ Wholly private
               | DealFlipDora Tile (Maybe Tile) -- ^ New dora, tile from wall to wanpai
               | DealNick Player Kaze Text
               | DealRiichi Kaze
               | DealEnded DealResults
               | GamePoints Player Int -- ^ New points
               deriving (Show, Read, Typeable)

-- | Actions you do on your turn.
data TurnAction = TurnTileDiscard Discard
                | TurnTileDraw Bool (Maybe Tile) -- ^ wanpai?, tile
                | TurnAnkan Tile
                | TurnShouminkan Tile
                | TurnTsumo
                deriving (Eq, Show, Read, Typeable)

-- | Actions you do on someone else's turn.
data GameAction = GameTurn TurnAction
                | GameShout Shout
                | GameDontCare -- ^ About shouting last discarded tile
                deriving (Eq, Show, Read, Typeable)

-- * Lenses

--
makeLenses ''Deal

-- * Game

type GameResults = Map Player Points

-- * Initialize state

fourPlayers :: [Player]
fourPlayers = Player <$> [0 .. 3]

-- | A new round with given player names.
newRound :: [Player] -- ^ Players, from Ton to Shaa
         -> [Text]   -- ^ Names
         -> IO Deal
newRound players names = do
    oja <- (players L.!!) <$> randomRIO (0, 3)
    dealTiles $ Deal
        { _pDeal          = 1
        , _pDeals         = []
        , _pDora          = []
        , _pFirstOja      = oja
        , _pHonba         = 0
        , _pRiichi        = 0
        , _pOja           = oja
        , _pPlayers       = mapFromList $ zip players (zip3 [Ton .. Pei] (repeat 25000) names)
        , _pResults       = Nothing
        , _pRound         = Ton
        , _pTurn          = Ton

        , _pWallTilesLeft = 0
        , _sEvents        = mempty
        , _sHands         = mempty
        , _sWall          = mempty
        , _sWanpai        = mempty
        , _sWaiting       = Nothing
        }

dealTiles :: Deal -> IO Deal
dealTiles deal = liftM dealTiles $ shuffleM riichiTiles
  where
    dealTiles tiles = deal
        { _pWallTilesLeft = length wall
        , _pDora          = [dora]
        , _sEvents        = []
        , _sHands         = Map.fromList $ zip [Ton .. Pei] (initHand <$> [h1, h2, h3, h4])
        , _sWall          = wall
        , _sWanpai        = wanpai
        } where
           (hands, xs)             = splitAt (13 * 4) tiles
           ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
           (dora : wanpai, wall)   = splitAt 14 xs
