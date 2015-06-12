------------------------------------------------------------------------------
-- | 
-- Module         : Mahjong.Kyoku.Internal
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Kyoku.Internal where

------------------------------------------------------------------------------
import           Import
import           Mahjong.Tiles
import           Mahjong.Configuration
import           Mahjong.Hand.Mentsu
------------------------------------------------------------------------------
import           Mahjong.Hand.Internal
------------------------------------------------------------------------------
import qualified Data.Map                       as Map
import qualified Data.List                      as L
import           System.Random.Shuffle          (shuffleM)
import           System.Random                  (randomRIO)
import qualified Text.PrettyPrint.ANSI.Leijen   as P
------------------------------------------------------------------------------

-- * Types

-- | Game state, including current round state.
--
-- Fields starting @_p@ are for public consumption and @_s@ for internal
-- only.
data Kyoku' m = Kyoku
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
    , _pResults       :: Maybe KyokuResults
    , _pDeals         :: [(Kaze, Int)] -- ^ Previous deals in decreasing order by time

    -- secret
    , _sEvents        :: [GameEvent]
    , _sHands         :: Map Kaze (Hand m)
    , _sWall          :: [Tile]
    , _sWanpai        :: [Tile]
    , _sWaiting       :: Maybe Waiting -- ^ Waiting turn action or shout(s)
    } deriving (Typeable)

deriving instance Show       (Kyoku' Maybe)
deriving instance Read       (Kyoku' Maybe)
deriving instance Show       (Kyoku' Identity)
deriving instance Read       (Kyoku' Identity)

type Kyoku = Kyoku' Identity

-- | Deal from a player's perspective
type AsPlayer = Kyoku' Maybe

-- | Left for turn, right for shout(s)
type Waiting = Either WaitTurnAction [WaitShout]

-- | (shouting player, shouting kaze, seconds left, available shouts)
type WaitShout = (Player, Kaze, Int, [Shout])
type WaitTurnAction = (Player, Kaze, Int, [Tile])

-- ** Actions and events

data GameEvent = DealStarts Player Kaze AsPlayer -- ^ Only at the start of a round
               | DealWaitForShout WaitShout -- ^ Number of seconds left to shout or confirm an ignore (See @GameDontCare@)
               | DealWaitForTurnAction WaitTurnAction
               | DealTurnBegins Kaze
               | DealTurnAction Kaze TurnAction
               | DealTurnShouted Kaze Shout -- ^ Who, Shout
               | DealPublicHandChanged Kaze HandP
               | DealPrivateHandChanged Player Kaze HandA -- ^ Wholly private
               | DealFlipDora Tile (Maybe Tile) -- ^ New dora, tile from wall to wanpai
               | DealNick Player Kaze Text
               | DealRiichi Kaze
               | DealEnded KyokuResults
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

-- ** Points, results

type PointsStatus = Map Player Points

newtype FinalPoints = FinalPoints PointsStatus deriving (Show, Read)

data KyokuResults = DealTsumo { dWinners :: [Winner], dPayers :: [Payer] }
                  | DealRon   { dWinners :: [Winner], dPayers :: [Payer] }
                  | DealDraw  { dTenpais :: [(Player, Points)], dNooten :: [Payer] }
                  | DealAbort { dReason :: AbortiveDraw }
                  deriving (Show, Read, Typeable)

data AbortiveDraw = Unrelated9
                  | SuufontsuRenta -- ^ All four winds
                  | SuuchaRiichi -- ^ All players riichi
                  | SuukanSanra -- ^ Fourth kon declared (or fifth if one player declared all four)
                  | Sanchahou -- ^ Three players ron
                  deriving (Show, Read, Typeable)

type Winner = (Player, Points, ValuedHand)
type Payer  = (Player, Points)

-- ** Hand value

-- | A hand that won.
data ValuedHand = ValuedHand
    { _vhMentsu :: [Mentsu]
    , _vhTiles  :: [Tile] -- ^ TODO: is this all tiles(?)
    , _vhValue  :: Value
    } deriving (Show, Read)

type Fu = Int

type Han = Int

type Points = Int

-- | Hand value
data Value = Value
    { _vaYaku  :: [Yaku]
    , _vaFu    :: Fu
    , _vaHan   :: Han
    , _vaValue :: Points -- ^ Basic points (non-dealer and not rounded)
    , _vaNamed :: Maybe Text
    } deriving (Show, Read)

data Yaku = Yaku
    { _yHan    :: Int -- TODO lens like naming to aeson/json
    , _yName   :: Text
    } deriving (Show, Read)

-- | Required info to calculate the value from a hand.
data ValueInfo = ValueInfo
    { _vKyoku  :: Kyoku
    , _vPlayer :: Kaze
    , _vHand   :: HandA
    } deriving (Show, Read)

-- * Construct state

fourPlayers :: [Player]
fourPlayers = Player <$> [0 .. 3]

-- | A new round with given player names.
newRound :: [Player] -- ^ Players, from Ton to Shaa
         -> [Text]   -- ^ Names
         -> IO Kyoku
newRound players names = do
    oja <- (players L.!!) <$> randomRIO (0, 3)
    dealTiles $ Kyoku
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

dealTiles :: Kyoku -> IO Kyoku
dealTiles deal = go <$> shuffleM riichiTiles
  where
    go tiles = deal
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

-- * Lenses

--
makeLenses ''Kyoku'
makeLenses ''ValueInfo
makeLenses ''ValuedHand
makeLenses ''Value
makeLenses ''Yaku

-- Instances

instance P.Pretty Kyoku where
    pretty Kyoku{..} = P.pretty _pDeals P.<$$>
        P.string "wall:"   P.<+> P.hang 0 (prettyList' _sWall) P.<$$>
        P.string "wanpai:" P.<+> P.hang 0 (prettyList' _sWanpai) P.<$$>
        P.string "hands:"  P.<+> P.hang 0 (P.list $ toList $ fmap P.pretty _sHands)
