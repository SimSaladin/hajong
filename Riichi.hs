{-# LANGUAGE ConstraintKinds, NoImplicitPrelude, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
module Riichi where

import ClassyPrelude
import Control.Lens
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as Map
import System.Random.Shuffle
import Tiles

-- * Types

type Player = Int
type Points = Int

data Shout = Pon | Kan | Chi | Ron
           deriving (Show, Read, Eq)

data HandPublic = HandPublic
                { _handOpen :: [Mentsu]
                , _handDiscards :: [(Tile, Maybe Player)]
                , _handRiichi :: Bool
                } deriving (Show, Read, Eq)

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handPick :: Maybe Tile
          , _handFuriten :: Maybe Bool -- ^ Just (temporary?)
          , _handPublic :: HandPublic
          } deriving (Show, Read, Eq)

data Game playerID = Game
                   { _gamePlayers :: Map Player playerID
                   , _gameState :: Maybe RiichiState -- maybe in running game
                   , _gamePoints :: Map Player Points
                   }

data RiichiSecret = RiichiSecret
                 { _riichiWall :: [Tile]
                 , _riichiWanpai :: [Tile]
                 , _riichiHands :: Map Player Hand
                 } deriving (Show, Read)

data RiichiPublic = RiichiPublic
                 { _riichiDora :: [Tile]
                 , _riichiRound :: Kazehai
                 , _riichiDealer :: Player
                 , _riichiPoints :: Map Player Points
                 , _riichiTurn :: Player
                 , _riichiEvents :: [Either Shout TurnAction]
                 } deriving (Show, Read)

data RiichiPlayer = RiichiPlayer
                  { _riichiPublic :: RiichiPublic
                  , _riichiPublicHands :: Map Player HandPublic
                  , _riichiHand :: Hand
                  } deriving (Show, Read)

type RiichiState = (RiichiSecret, RiichiPublic)

data TurnAction = TurnRiichi Tile
                | TurnDiscard Tile
                | TurnDraw Bool (Maybe Tile)
                | TurnAnkan Tile
                | TurnShouted Shout Player -- shout [by who]
                deriving (Show, Read)

data RoundEvent = RoundAction Player TurnAction
                | RoundTsumo Player
                | RoundRon Player [Player] -- From, who?
                | RoundDraw [Player] -- tenpai players

type GameMonad m = ( MonadReader RiichiPublic m
                   , MonadState RiichiSecret m
                   , MonadWriter RoundEvent m
                   , MonadError Text m
                   )

liftE :: GameMonad m => (a -> m b) -> Either Text a -> m b
liftE = either throwError

makeLenses ''Game
makeLenses ''RiichiSecret
makeLenses ''RiichiPublic
makeLenses ''RiichiPlayer
makeLenses ''HandPublic
makeLenses ''Hand

-- | State visible to the player
riichiPlayer :: RiichiState -> Player -> Maybe RiichiPlayer
riichiPlayer (secret, public) player =
    secret ^. riichiHands . at player <&> RiichiPlayer public 

handOf' :: GameMonad m => Player -> m Hand
handOf' player = use (handOf player) >>= maybe (throwError "Player not found") return

-- | huh?
handOf :: Functor f => Int -> (Maybe Hand -> f (Maybe Hand)) -> RiichiSecret -> f RiichiSecret
handOf player = riichiHands.at player

-- * Initialize

initHand :: [Tile] -> Hand
initHand tiles = Hand tiles Nothing Nothing $ HandPublic [] [] False

newGame :: RiichiPublic
newGame = RiichiPublic
    { _riichiDora   = []
    , _riichiRound  = Ton
    , _riichiDealer = 1
    , _riichiPoints = Map.fromList $ zip [1..] (replicate 4 25000)
    , _riichiTurn   = 1
    , _riichiEvents = []
    }

newRiichiState :: IO RiichiState
newRiichiState = liftM (`setSecret` newGame) newSecret

setSecret :: RiichiSecret -> RiichiPublic -> RiichiState
setSecret secret public =
    (secret & set riichiWanpai wanpai', public & set riichiDora [dora])
    where
        (dora : wanpai') = secret ^. riichiWanpai

newSecret :: IO RiichiSecret
newSecret = liftM dealTiles $ shuffleM riichiTiles
    where
        dealTiles tiles = RiichiSecret
            { _riichiWall = wall
            , _riichiWanpai = wanpai
            , _riichiHands = Map.fromList $ zip [1..] (map initHand [h1, h2, h3, h4])
            } where
                (hands, xs)             = splitAt (13 * 4) tiles
                ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
                (wanpai, wall)          = splitAt 14 xs

defaultPlayers :: [Player]
defaultPlayers = [1..4]

-- * Game operations

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound (_, public) = do
    secret <- newSecret
    return $ setSecret secret $ public & set riichiTurn (public ^. riichiDealer)

-- | Apply an action on current player's turn
runTurn :: GameMonad m => TurnAction -> m ()
runTurn action = do
    player <- view riichiTurn
    hand <- use (handOf player) >>= maybe (throwError "Hand of current player not found") return

    case action of
        TurnRiichi tile         -> liftE (handOf player ?=) $ setRiichi tile hand
        TurnDiscard tile        -> liftE (handOf player ?=) $ discard tile hand
        TurnDraw dead Nothing   -> undefined
        TurnDraw _ _            -> throwError "Draw action cannot specify the tile"
        TurnAnkan tile          -> undefined
        TurnShouted shout shouter -> undefined

    tell $ RoundAction player action -- here only if no error was thrown

-- * Hand operations

-- | Discard a tile; returns Left err if discard is not possible due to the
-- tile not being in the hand, or due to riichi.
discard :: Tile -> Hand -> Either Text Hand
discard tile hand
    | hand ^. handPick == Just tile = Right $ hand & set handPick Nothing . setDiscard
    | hand ^. handPublic.handRiichi = Left "Cannot change wait in riichi"
    | otherwise                     = case ys of
        []      -> Left "Tile not in hand"
        (_:ys') -> Right $ hand & set handConcealed (xs ++ ys') . setDiscard
    where
        (xs, ys) = break (== tile) (_handConcealed hand)
        setDiscard = over (handPublic.handDiscards) (++ [(tile, Nothing)])

setRiichi :: Tile -> Hand -> Either Text Hand
setRiichi tile hand
    | hand ^. handPublic.handRiichi = Left "Already in riichi"
    | tenpai hand                   = Right $ set (handPublic.handRiichi) True hand
    | otherwise                     = Left "Not in tenpai"

tenpai :: Hand -> Bool
tenpai hand = True -- TODO implement

-- * Mentsu

-- * PrettyPrint
--
--     mm-mm-mm
--     mm-mm-mm                                    mm-mm-mm
--     mm-mm-mm    N       Player1                 mm-mm-mm-mm
--     mm-mm-mm-mm _ _ _ _ _ _ _ _ _ _ _ _ _       mm-mm-mm-mm
--                                                 mm-mm-mm
--  E        |    XX XX XX XX XX XX XX XX      XX  |
--           |          XX XX XX XX XX XX      XX  | W
--           |          XX XX XX XX XX XX      XX  |
--           |  XX XX XX                 XX XX XX  |  
--   Player3 |  XX XX XX  (NN)           XX XX XX  | Player4
--           |  XX XX XX                 XX XX XX  |  
--   (25000) |  XX XX XX Do Ra He Re ..  XX XX XX  | (25000)
--           |  XX XX XX                 XX XX XX  |  
--           |  XX XX XX                 XX XX XX  |  
--           |  XX XX XX                 XX XX XX  |  
--           |  XX      XX XX XX XX XX XX          |
--    mm-mm-mm  XX      XX XX XX XX XX XX          |
--    mm-mm-mm  XX      XX XX XX XX XX XX XX XX XX |
--    mm-mm-mm
-- mm-mm-mm-mm 
--
--            01 02 03 04 05 06 07 08 09 10 11 12 13   14
--                                            mm-mm-mm-mm
--                 S Player3      (25000)     mm-mm-mm
--                                            mm-mm-mm
--                                            mm-mm-mm
--  Parts:  - mentsu (ankan, open)
--          - discard pile
--          - dead wall (NN)
--          - Dora indicators
--          - player avatars, points, seat winds



