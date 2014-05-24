module Riichi where

import ClassyPrelude
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Random.Shuffle

import GameTypes

-- | Context of game and deal flow.
type GameMonad m = ( MonadReader RiichiPublic m
                   , MonadState RiichiSecret m
                   , MonadWriter [RoundEvent] m
                   , MonadError Text m
                   )

type GameMonad' = RWST RiichiPublic [RoundEvent] RiichiSecret (Either Text)

-- | Run a GameMonad' action on a RiichiState (specialized)
gsAction' ::  GameMonad' a -> GameServer pid -> Either Text (a, RiichiSecret, [RoundEvent])
gsAction' m gs = case _gameState gs of
    Just (secret, public) -> runRWST m public secret
    Nothing               -> Left "No deal"

liftE :: GameMonad m => Either Text a -> m a
liftE = either throwError return

handOf' :: GameMonad m => Player -> m Hand
handOf' player = use (handOf player) >>= maybe (throwError "Player not found") return

defaultPlayers :: [Player]
defaultPlayers = [Player Ton .. Player Pei]

-- * GameServer

newGameServer :: Text -> GameServer a
newGameServer name = GameServer players name Nothing
    where
        players = zip3 defaultPlayers (repeat Nothing) (repeat 25000)

-- | Return the action to create a new game in the server state, unless
-- a game is still running (gameState ~ Just)
gsNewGame :: GameServer a -> Maybe (IO (GameServer a))
gsNewGame gs = maybe (Just $ newRiichiState <&> flip (set gameState) gs . Just) (const Nothing) (_gameState gs)

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

-- * Game logic

runTurn :: GameMonad m => Player -> TurnAction -> m ()
runTurn player action = do
    turnPlayer <- view riichiTurn
    when (turnPlayer /= player) $ throwError "Not your turn"

    hand <- use (handOf player) >>= maybe (throwError "Hand of current player not found (shouldn't happen?)") return

    case action of
        TurnRiichi tile           -> liftE (setRiichi tile hand) >>= (handOf player ?=)
        TurnDiscard tile          -> liftE (discard tile hand)   >>= (handOf player ?=)
        TurnAnkan tile            -> liftE (doAnkan tile hand) >>= (handOf player ?=)
        TurnShouted shout shouter -> undefined
        TurnDraw False Nothing    -> drawWall hand >>= (handOf player ?=)
        TurnDraw _ _              -> throwError "Draw action cannot specify the tile"

    Just hand' <- use (handOf player)
    when (hand /= hand') $ tell [RoundPrivateHand player hand]
    when (_handPublic  hand /= _handPublic hand') $ tell [RoundPublicHand player $ _handPublic hand']

    tell [RoundAction player action]

drawWall :: GameMonad m => Hand -> m Hand
drawWall hand = do
    wall <- use riichiWall
    case wall of
        (x:xs) -> do riichiWall .= xs
                     return $ set handPick (Just x) hand
        _ -> throwError "No tiles left"

-- * Deal state

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound (_, public) = do
    secret <- newSecret
    return $ setSecret secret $ public & set riichiTurn (public ^. riichiDealer)

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
