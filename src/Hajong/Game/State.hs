------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.State
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Game.State where

import ClassyPrelude
import Control.Lens
import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Maybe

import Hajong.Game.Round
import Hajong.Game.Hand
import Hajong.Game.Tiles
import Hajong.Game.Types

-- | This conforms to "GameMonad"
type GameRWST = RWST RiichiPublic [RoundEvent] RiichiSecret (Either Text)

runGS ::  GameRWST a -> GameServer pid -> Either Text (a, RiichiSecret, [RoundEvent])
runGS m gs = case _gameState gs of
    Just state -> runRWST m (_riichiPublic state) (_riichiSecret state)
    Nothing    -> Left "No active deal"

newRiichiState :: IO RiichiState
newRiichiState = do
    secret <- newSecret
    let (dora:wanpai) = _riichiWanpai secret
    return $ RiichiState
        { _riichiSecret = secret { _riichiWanpai = wanpai }
        , _riichiPublic = RiichiPublic
            { _riichiDora          = [dora]
            , _riichiWallTilesLeft = 0
            , _riichiRound         = Ton
            , _riichiDealer        = Player Ton
            , _riichiTurn          = Player Ton
            , _riichiPoints        = Map.fromList $ zip defaultPlayers (repeat 25000)
            , _riichiEvents        = []
            }
        }

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

-- | Nothing if game full
gsAddPlayer :: Eq a => a -> GameServer a -> Maybe (GameServer a)
gsAddPlayer a gs = do
    p <- findFree gs
    return $ gs & over (gamePlayers.each)
        (if' <$> view (_1.to (==p)) <*> set _2 (Just a) <*> id)
    where
        findFree = fmap (view _1) . find (isn't _Just.view _2) . view gamePlayers

-- ** Functions

-- | Build the state visible to the player
gsPlayerLookup :: GameServer id -> Player -> Maybe (GamePlayer id)
gsPlayerLookup game player = game^.gameState^?_Just.to build
    where
        build = GamePlayer
            <$> pure player
            <*> view riichiPublic
            <*> view (riichiSecret.riichiHands.to (map _handPublic))
            <*> pure (game^.gamePlayers)
            <*> view (riichiSecret.riichiHands.at player.to fromJust)

