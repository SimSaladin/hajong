{-# LANGUAGE ConstraintKinds, NoImplicitPrelude, TemplateHaskell, OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Riichi where

import ClassyPrelude
import Control.Lens
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Random.Shuffle
import Tiles

if' :: Bool -> t -> t -> t
if' cond th el = if cond then th else el


-- * Game types

-- | Server-side state
data GameServer playerID = GameServer
                   { _gamePlayers :: RiichiPlayers playerID
                   , _gameName :: Text
                   , _gameState :: Maybe RiichiState -- maybe in running game
                   }

newtype Player = Player Kazehai deriving (Show, Read, Eq, Ord)
deriving instance Enum Player

type RiichiPlayers playerID = [(Player, Maybe playerID, Points)]

type Points = Int

-- * Single riichi deal types

type RiichiState = (RiichiSecret, RiichiPublic)

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
                 , _riichiEvents :: [Either Shout TurnAction]
                 } deriving (Show, Read)

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

-- * Player-specific

-- | State of single player
data GamePlayer playerID = GamePlayer
                  { _playerPlayer :: Player
                  , _playerPublic :: RiichiPublic
                  , _playerPublicHands :: Map Player HandPublic
                  , _playerPlayers :: RiichiPlayers playerID
                  , _playerMyHand :: Hand
                  } deriving (Show, Read)

-- * Hands' types

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


-- * Utility

-- * Lenses
makeLenses ''GameServer
makeLenses ''GamePlayer
makeLenses ''RiichiSecret
makeLenses ''RiichiPublic
makeLenses ''HandPublic
makeLenses ''Hand

defaultPlayers :: [Player]
defaultPlayers = [Player Ton .. Player Pei]

-- * GameMonad

-- | Context of game and deal flow.
type GameMonad m = ( MonadReader RiichiPublic m
                   , MonadState RiichiSecret m
                   , MonadWriter RoundEvent m
                   , MonadError Text m
                   )

liftE :: GameMonad m => (a -> m b) -> Either Text a -> m b
liftE = either throwError

handOf' :: GameMonad m => Player -> m Hand
handOf' player = use (handOf player) >>= maybe (throwError "Player not found") return

-- | huh?
handOf :: Functor f => Player -> (Maybe Hand -> f (Maybe Hand)) -> RiichiSecret -> f RiichiSecret
handOf player = riichiHands.at player


-- * Game state

newGameServer :: Text -> GameServer a
newGameServer name = GameServer players name Nothing
    where
        players = zip3 defaultPlayers (repeat Nothing) (repeat 25000)

-- | Nothing if game full
gsAddPlayer :: Eq a => a -> GameServer a -> Maybe (GameServer a)
gsAddPlayer a gs = do
    p <- findFree gs
    return $ gs & over (gamePlayers.each)
        (if' <$> view (_1.to (==p)) <*> set _2 (Just a) <*> id)
    where
        findFree = fmap (view _1) . find (isn't _Just.view _2) . view gamePlayers

-- | Build the state visible to the player
gsPlayerLookup :: GameServer id -> Player -> Maybe (GamePlayer id)
gsPlayerLookup game player = game^.gameState^?_Just.to build
    where
        build = GamePlayer
            <$> pure player
            <*> view _2
            <*> view (_1.riichiHands.to (map _handPublic))
            <*> pure (game^.gamePlayers)
            <*> view (_1.riichiHands.at player.to fromJust)

-- | Return the action to create a new game in the server state, unless
-- a game is still running (gameState ~ Just)
gsNewGame :: GameServer a -> Maybe (IO (GameServer a))
gsNewGame gs = maybe (Just $ newRiichiState <&> flip (set gameState) gs . Just) (const Nothing) (_gameState gs)



-- * Deal state

newRiichiState :: IO RiichiState
newRiichiState = liftM (`setSecret` newGame) newSecret

-- | Four-player riichi game
newGame :: RiichiPublic
newGame = RiichiPublic
    { _riichiDora          = []
    , _riichiWallTilesLeft = 0
    , _riichiRound         = Ton
    , _riichiDealer        = Player Ton
    , _riichiTurn          = Player Ton
    , _riichiPoints        = Map.fromList $ zip defaultPlayers (repeat 25000)
    , _riichiEvents        = []
    }

setSecret :: RiichiSecret -> RiichiPublic -> RiichiState
setSecret secret public =
    ( secret & set riichiWanpai wanpai'
    , public & set riichiDora [dora] & set riichiWallTilesLeft (secret ^. riichiWall.to length)
    ) where
        (dora : wanpai') = secret ^. riichiWanpai

newSecret :: IO RiichiSecret
newSecret = liftM dealTiles $ shuffleM riichiTiles
    where
        dealTiles tiles = RiichiSecret
            { _riichiWall = wall
            , _riichiWanpai = wanpai
            , _riichiHands = Map.fromList $ zip defaultPlayers (map initHand [h1, h2, h3, h4])
            } where
                (hands, xs)             = splitAt (13 * 4) tiles
                ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
                (wanpai, wall)          = splitAt 14 xs


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

initHand :: [Tile] -> Hand
initHand tiles = Hand tiles Nothing Nothing $ HandPublic [] [] False

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
