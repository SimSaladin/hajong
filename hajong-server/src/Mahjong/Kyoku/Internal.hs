{-# LANGUAGE DeriveGeneric #-}
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

import           Import
import           Mahjong.Tiles
import           Mahjong.Configuration
import           Mahjong.Hand.Mentsu
import           Mahjong.Kyoku.Flags
------------------------------------------------------------------------------
import           Mahjong.Hand.Internal
import           Mahjong.Hand.Algo
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
data Kyoku = Kyoku
    -- always public
    { _pRound         :: Round
    , _pTurn          :: Kaze   -- ^ Current player in turn
    , _pOja           :: Player -- ^ TODO is field this necessary?
    , _pFirstOja      :: Player
    , _pWallTilesLeft :: Int    -- ^ A public aggregate of sWall
    , _pDora          :: [Tile] -- ^ Indicators
    , _pPlayers       :: Map Kaze (Player, Points, Text)
    , _pHonba         :: Int
    , _pRiichi        :: Int -- ^ Points in table for riichi
    , _pResults       :: Maybe KyokuResults
    , _pFlags         :: Set Flag -- ^ List of extendable flags active in the game.

    -- secret
    , _sEventHistory  :: [GameEvent] -- ^ Complete history of events applied in this kyoku, latest first
    , _sHands         :: Map Kaze Hand
    , _sWall          :: [Tile]
    , _sWanpai        :: Wanpai
    , _sWaiting       :: Maybe Waiting -- ^ Waiting turn action or shout(s)
    } deriving (Typeable, Show, Read, Generic)

-- | Kyoku viewed from a specific player's point-of-view
data PlayerKyoku = PlayerKyoku Player Kaze Kyoku
                 deriving (Typeable, Show, Read)

-- | Left for turn, right for shout(s)
type Waiting = Either WaitTurnAction [WaitShout]

-- | @E1 == (Ton, 1, 0)@ etc. last is for renchans.
type Round = (Kaze, Int, Int)

-- | (shouting player, shouting kaze, secs_until_auto, shout)
type WaitShout = (Player, Kaze, Int, [Shout])

-- | When waiting for a turn action: (player, player_kaze, secs_until_auto, tiles_to_riichi_with)
type WaitTurnAction = (Player, Kaze, Int, [Tile])

-- ^ Indices 0-3 are kan supplement tiles. indices 4-8 are dora, 8-12 ura-dora.
data Wanpai = Wanpai
    { _wSupplement :: [Tile]
    , _wDora       :: [Tile]
    , _wUraDora    :: [Tile]
    , _wBlank      :: [Tile]
    } deriving (Typeable, Eq, Show, Read)

-- ** Actions and events

data GameEvent = DealStarts PlayerKyoku -- ^ Only at the start of a round
               | DealWaitForShout WaitShout -- ^ Number of seconds left to shout or confirm an ignore (See @GameDontCare@)
               | DealWaitForTurnAction WaitTurnAction
               | DealTurnBegins Kaze
               | DealTurnAction Kaze TurnAction
               | DealTurnShouted Kaze Shout -- ^ Who, Shout
               | DealPublicHandChanged Kaze PlayerHand
               | DealPrivateHandChanged Player Kaze Hand -- ^ Wholly private
               | DealFlipDora Tile -- ^ New dora was flipped
               | DealNick Kaze Player Text -- Pos, player id, nick TODO: no nick but fetch the player info separetely
               | DealRiichi Kaze
               | DealEnded KyokuResults
               | GamePoints Kaze Int -- ^ Point change
               | GameEnded FinalPoints
               deriving (Show, Read, Typeable)

-- | Actions you do on your turn.
data TurnAction = TurnTileDiscard Discard
                | TurnTileDraw Bool (Maybe Tile) -- ^ wanpai?, tile
                | TurnAnkan Tile
                | TurnShouminkan Tile
                | TurnTsumo
                deriving (Show, Read, Typeable)

-- | A @GamAction@ is what clients send to interact in the game.
data GameAction = GameTurn TurnAction -- ^ An action of the player in turn
                | GameShout Shout -- ^ Call after a discard
                | GameDontCare -- ^ Discard shouts from this player
                deriving (Show, Read, Typeable)

-- ** Points, results

type PointsStatus = Map Player Points

-- | Results from a whole game of mahjong.
newtype FinalPoints = FinalPoints PointsStatus deriving (Show, Read, Eq, Generic)

data KyokuResults = DealTsumo { dWinners :: [Winner], dPayers :: [Payer] }
                  | DealRon   { dWinners :: [Winner], dPayers :: [Payer] }
                  | DealDraw  { dTenpais :: [Tenpai], dNooten :: [Payer] }
                  | DealAbort { dReason :: AbortiveDraw }
                  deriving (Eq, Show, Read, Typeable)

data AbortiveDraw = KuushuuKyuuhai -- ^ Nine unrelated tiles in initial hand
                  | SuufonRenda    -- ^ All four winds
                  | SuuchaRiichi   -- ^ All players riichi
                  | SuuKaikan      -- ^ Fourth kon declared (or fifth if one player declared all four)
                  | Sanchahou      -- ^ Three players ron
                  deriving (Eq, Show, Read, Typeable)

type Winner = (Kaze, Points, ValuedHand)
type Tenpai = (Kaze, Points, [Mentsu], [Tile])
type Payer  = (Kaze, Points)

-- ** Hand value

-- | A hand that won.
data ValuedHand = ValuedHand
    { _vhMentsu :: [Mentsu]
    , _vhTiles  :: [Tile] -- ^ Concealed tiles
    , _vhValue  :: Value
    } deriving (Eq, Show, Read)

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
    } deriving (Eq, Show, Read)

data Yaku = Yaku
    { _yHan    :: Int
    , _yName   :: Text
    } | YakuExtra
    { _yHan    :: Int
    , _yName   :: Text
    } deriving (Eq, Ord, Show, Read)

-- | Required info to calculate the value from a hand.
data ValueInfo = ValueInfo
    { _vKyoku  :: Kyoku
    , _vPlayer :: Kaze
    , _vHand   :: Hand
    } deriving (Show, Read)

instance HasGroupings ValueInfo where getGroupings = getGroupings . _vHand

-- * Construct state

fourPlayers :: [Player]
fourPlayers = Player <$> [0 .. 3]

-- | A new round with given player names.
newKyoku :: [Player] -- ^ Players, from Ton to Shaa
         -> [Text]   -- ^ Names
         -> IO Kyoku
newKyoku players names = do
    oja <- (players L.!!) <$> randomRIO (0, 3)
    tiles <- shuffleTiles
    return $ dealTiles tiles $ Kyoku
        { _pDora          = []
        , _pFirstOja      = oja
        , _pHonba         = 0
        , _pRiichi        = 0
        , _pOja           = oja
        , _pPlayers       = mapFromList $ zip [Ton .. Pei] (zip3 players (repeat 25000) names)
        , _pResults       = Nothing
        , _pRound         = (Ton, 1, 0)
        , _pTurn          = Ton
        , _pFlags         = setFromList [FirstRoundUninterrupted]

        , _pWallTilesLeft = 0
        , _sEventHistory  = mempty
        , _sHands         = mempty
        , _sWall          = mempty
        , _sWanpai        = Wanpai mempty mempty mempty mempty
        , _sWaiting       = Nothing
        }

dealTiles :: [Tile] -> Kyoku -> Kyoku
dealTiles tiles deal = deal
        { _pWallTilesLeft = length wall
        , _pDora          = [doraX]
        , _sEventHistory  = []
        , _sHands         = Map.fromList $ zip [Ton .. Pei] (initHand <$> [h1, h2, h3, h4])
        , _sWall          = wall
        , _sWanpai        = Wanpai supplement doraXS uradora rest
        } where
           (hands, xs)             = splitAt (13 * 4) tiles
           ((h1, h2), (h3, h4))    = splitAt 13 *** splitAt 13 $ splitAt (13*2) hands
           (wanpai, wall)          = splitAt 14 xs
           ((supplement, doraX:doraXS), (uradora, rest))
                                   = splitAt 4 *** splitAt 5 $ splitAt 9 wanpai

shuffleTiles :: IO [Tile]
shuffleTiles = shuffleM riichiTiles

-- * Lenses

--
makeLenses ''Kyoku
makeLenses ''Wanpai
makeLenses ''ValueInfo
makeLenses ''ValuedHand
makeLenses ''Value
makeLenses ''Yaku

-- * Utility

kyokuTiles :: Kyoku -> [Tile]
kyokuTiles kyoku = kyoku^.pDora ++ kyoku^.sWall ++ kyoku^..sHands.each.handConcealed.each ++ kyoku^.sWanpai.to wanpaiTiles

-- | Get all tiles currently in the wanpai in no particular order.
wanpaiTiles :: Wanpai -> [Tile]
wanpaiTiles Wanpai{..} = _wSupplement ++ _wDora ++ _wUraDora ++ _wBlank

-- Instances

instance P.Pretty Kyoku where
    pretty Kyoku{..} =
        P.string "wall:"   P.<+> P.hang 0 (prettyList' _sWall) P.<$$>
        P.string "unrevealed dora:" P.<+> P.hang 0 (prettyList' $ _wDora _sWanpai) P.<$$>
        P.string "hands:"  P.<+> P.hang 0 (P.list $ toList $ fmap P.pretty _sHands)

$(deriveSafeCopy 0 'base ''GameEvent)
$(deriveSafeCopy 0 'base ''Kyoku)
$(deriveSafeCopy 0 'base ''PlayerKyoku)
$(deriveSafeCopy 0 'base ''TurnAction)
$(deriveSafeCopy 0 'base ''Wanpai)
$(deriveSafeCopy 0 'base ''KyokuResults)
$(deriveSafeCopy 0 'base ''Flag)
$(deriveSafeCopy 0 'base ''FinalPoints)
$(deriveSafeCopy 0 'base ''ValuedHand)
$(deriveSafeCopy 0 'base ''Value)
$(deriveSafeCopy 0 'base ''Yaku)
$(deriveSafeCopy 0 'base ''AbortiveDraw)
