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

type RiichiPlayers player = Map Kaze (Player, player, Points)

-- * Deal

-- | Game state, including current round state.
--
-- Fields starting @_p@ are for public consumption and @_s@ for internal
-- only.
data Deal = Deal
    { _pRound         :: Kaze
    , _pDeal          :: Int
    , _pDora          :: [Tile]
    , _pFirstOja      :: Player
    , _pHonba         :: Int
    , _pOja           :: Player
    , _pPlayers       :: Map Player (Kaze, Points, Text)
    , _pResults       :: Maybe DealResults
    , _pTurn          :: Kaze
    , _pDeals         :: [(Kaze, Int)] -- ^ Previous deals in decreasing order by time
    , _pWallTilesLeft :: Int

    -- secret
    , _sEvents        :: [GameEvent]
    , _sHands         :: Map Kaze Hand
    , _sWaitingShouts :: [(Kaze, Shout)]
    , _sWall          :: [Tile]
    , _sWanpai        :: [Tile]
    } deriving (Show, Read, Typeable)

-- Pretty instances 

instance P.Pretty Deal where
    pretty Deal{..} = P.pretty _pDeals P.<$$>
        P.string "wall:"   P.<+> P.hang 0 (prettyList' _sWall) P.<$$>
        P.string "wanpai:" P.<+> P.hang 0 (prettyList' _sWanpai) P.<$$>
        P.string "hands:"  P.<+> P.hang 0 (P.list $ toList $ fmap P.pretty _sHands)


-- | State of single player. Note that there is no RiichiSecret.
data GamePlayer = GamePlayer
                { _playerKaze :: Kaze
                , _playerPlayer :: Player -- ^ Me
                , _playerName :: Text
                , _playerPublic :: Deal
                , _playerPublicHands :: Map Kaze HandPublic
                , _playerMyHand :: Hand
                } deriving (Show, Read, Typeable)

-- * Actions and events

data GameEvent = DealPrivateStarts GamePlayer -- ^ Only at the start of a round
               | DealPrivateWaitForShout Player Int [Shout] -- ^ Number of seconds left to shout or confirm an ignore (See @GameDontCare@)
               | DealPrivateWaitForTurnAction Player Int
               | DealPrivateChange Player Hand
               | DealTurnBegins Kaze
               | DealTurnAction Kaze TurnAction
               | DealTurnShouted Kaze Shout -- ^ Who, Shout
               | DealHandChanged Kaze HandPublic
                        -- TODO this is a bit too vague to be exactly
                        -- useful for clients.. perhaps could identify
                        -- between draws, kans, shouts.
               | DealEnded DealResults
               | DealNick Player Kaze Text
               deriving (Show, Read, Typeable)

-- | Actions you do on your turn.
data TurnAction = TurnTileDiscard Bool Tile -- ^ Riichi?
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
makeLenses ''GamePlayer

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
        , _pOja           = oja
        , _pPlayers       = mapFromList $ zip players (zip3 [Ton .. Pei] (repeat 25000) names)
        , _pResults       = Nothing
        , _pRound         = Ton
        , _pTurn          = Ton

        , _pWallTilesLeft = 0
        , _sEvents        = mempty
        , _sHands         = mempty
        , _sWaitingShouts = mempty
        , _sWall          = mempty
        , _sWanpai        = mempty
        }

dealTiles :: Deal -> IO Deal
dealTiles deal = liftM dealTiles $ shuffleM riichiTiles
  where
    dealTiles tiles = deal
        { _pWallTilesLeft = length wall
        , _pDora          = [dora]
        , _sEvents        = []
        , _sHands         = Map.fromList $ zip [Ton .. Pei] (initHand <$> [h1, h2, h3, h4])
        , _sWaitingShouts = []
        , _sWall          = wall
        , _sWanpai        = wanpai
        } where
           (hands, xs)             = splitAt (13 * 4) tiles
           ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
           (dora : wanpai, wall)   = splitAt 14 xs
