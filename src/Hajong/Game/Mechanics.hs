{-# LANGUAGE DeriveFunctor #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Mechanics
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- The mechanics to play a game of mahjong.
module Hajong.Game.Mechanics where

import           Control.Monad.RWS
import qualified Data.Map as Map

import Hajong.Game.Round
import Hajong.Game.Tiles

-- | "GameState" records all information of a single game.
data GameState playerID = GameState
                   { _gamePlayers :: Map Player (Maybe playerID)
                   , _gameName :: Text
                   , _gameRound :: Maybe RiichiState -- maybe in running game
                   } deriving (Show, Read, Functor)
makeLenses ''GameState

type RoundM' = RWST RiichiPublic [GameEvent] RiichiSecret (Either Text)

-- * GameState

-- | Create a new GameState with the given label.
newEmptyGS :: Text -> GameState a
newEmptyGS name = GameState players name Nothing
    where
        players = Map.fromList $ zip [minBound .. maxBound] (repeat Nothing)

-- | Execute a round action in the "GameState".
--
-- Succesfull return value contains the value from the run "RoundM" action,
-- arbitrarily modified "RiichiSecret" and public changes encoded in
-- "GameEvents".
--
-- RoundM-actions do not explicitly modify the public state (RiichiPublic),
-- so **it is important you apply the changes implied by the events on the
-- state!** Haskell clients may use "@applyRoundEvents@".
runRoundM :: RoundM' r -> GameState p -> Either Text (r, RiichiSecret, [GameEvent])
runRoundM m = maybe (Left "No active round!") run . _gameRound
    where run rs = runRWST m (_riichiPublic rs) (_riichiSecret rs)

-- | Return an IO action to create the next round if
--      - it would be first round and all player seats are occupied, or
--      - the previous round has ended. (TODO!)
maybeNextRound :: GameState a -> Maybe (IO (GameState a))
maybeNextRound gs = do
    guard . null $ gs^.gamePlayers^..each._Nothing
    case _gameRound gs of
        Nothing -> Just $ (\rs -> gs & gameRound .~ Just rs) <$> newRiichiState
        Just _  -> Nothing

-- | Try putting the given client to an empty player seat. Returns Nothing
-- if the game is already full.
addClient :: Eq a => a -> GameState a -> Maybe (GameState a)
addClient client = uncurry (flip (<$)) . mapAccumLOf (gamePlayers.traversed) go Nothing
    where
        go Nothing Nothing = (Just (), Just client)
        go      s        c = (s, c)

removeClient :: Eq a => a -> GameState a -> Maybe (GameState a)
removeClient client gs = do
    p <- clientToPlayer client gs
    return $ (gamePlayers.at p .~ Nothing) gs

playerToClient :: GameState c -> Player -> Maybe c
playerToClient gs p = gs^.gamePlayers.at p.to join

clientToPlayer :: Eq c => c -> GameState c -> Maybe Player
clientToPlayer c gs = gs^.gamePlayers & ifind (\_ x -> x == Just c) <&> view _1
