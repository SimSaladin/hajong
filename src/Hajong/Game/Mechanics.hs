------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Mechanics
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.Mechanics where

import ClassyPrelude
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import qualified Data.Map as Map
import qualified Data.List as L
import Data.Maybe (fromJust)
import System.Random.Shuffle

import Hajong.Game.Types

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
gsNewGame gs = do
    -- all player seats occupied
    guard (gs^.gamePlayers & find (isn't _Just.view _2) & isNothing)

    case _gameState gs of
        Nothing -> return $ (\rs -> gs & set gameState (Just rs)) <$> newRiichiState
        Just _ -> undefined -- TODO Check if round is over?

-- | Execute an action in the "GameServer".
gsAction ::  RoundM' a -> GameServer pid -> Either Text (a, RiichiSecret, [RoundEvent])
gsAction m gs = case _gameState gs of
    Just (secret, public) -> runRWST m public secret
    Nothing               -> Left "No deal"

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

-- * Round state

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

-- * Round events

runTurn :: RoundM m => Player -> TurnAction -> m (Maybe Hand)
runTurn player action = do
    turnPlayer <- view riichiTurn
    when (turnPlayer /= player) $ throwError "Not your turn"

    hand <- use (handOf player) >>= maybe (throwError "Hand of current player not found (shouldn't happen?)") return
    newHand <- case action of
        TurnRiichi tile           -> liftE (setRiichi tile hand)
        TurnDiscard tile          -> liftE (discard tile hand)
        TurnAnkan tile            -> liftE (doAnkan tile hand)
        TurnShouted shout shouter -> liftE (doShout shout player hand) >>= processShout player shouter
        TurnDraw False Nothing    -> drawWall hand
        TurnDraw True  Nothing    -> drawDeadWall hand
        TurnDraw _ _              -> throwError "Draw action cannot specify the tile"

    handOf player ?= newHand

    Just hand' <- use (handOf player)
    tell [RoundAction player action] -- TODO hide private
    when (_handPublic hand /= _handPublic hand') $ tell [RoundPublicHand player $ _handPublic hand']

    return $ if hand /= hand' then Just hand else Nothing

processShout :: RoundM m => Player -> Player -> (Hand, Player -> Hand -> Maybe (Either () Mentsu)) -> m Hand
processShout turnPlayer shouter (turnHand, f) = do
    hand <- use (handOf shouter) >>= maybe (throwError "Hand not fonud") return
    case f shouter hand of
        Nothing             -> throwError "Shout would be invalid"
        Just (Left ())      -> tell [RoundRon turnPlayer [shouter]]
        Just (Right mentsu) -> do
            let newHand = hand & over (handPublic.handOpen) (mentsu :)
                               . over handConcealed
                               (L.\\ mentsuPai mentsu)
            handOf shouter ?= newHand
            tell [RoundPublicHand shouter $ _handPublic hand]
    return turnHand

drawWall :: RoundM m => Hand -> m Hand
drawWall hand = do
    wall <- use riichiWall
    case wall of
        (x:xs) -> do riichiWall .= xs
                     return $ set handPick (Just x) hand
        _ -> throwError "No tiles left"

drawDeadWall :: RoundM m => Hand -> m Hand
drawDeadWall hand = do
    wall <- use riichiWanpai
    undefined
