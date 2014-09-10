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
module Mahjong.Mechanics where

import           Control.Monad.RWS
import qualified Data.Map as Map

import Mahjong.Round
import Mahjong.Tiles

-- | "GameState" records all information of a single game.
data GameState playerID = GameState
                   { _gamePlayers :: Map Player playerID
                   , _gameName :: Text
                   , _gameRound :: Maybe RiichiState -- maybe in running game
                   } deriving (Show, Read, Functor)
makeLenses ''GameState

type RoundM' = RWST RiichiPublic [GameEvent] RiichiSecret (Either Text)

-- * GameState

-- | Create a new GameState with the given label.
newEmptyGS :: playerID -> Text -> GameState playerID
newEmptyGS defaultPlayer name = GameState players name Nothing
    where
        players = Map.fromList $ zip [minBound .. maxBound] (repeat defaultPlayer)

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
maybeNextRound :: (a -> Bool) -> GameState a -> Maybe (IO (GameState a))
maybeNextRound ready gs =
    case _gameRound gs of
        Nothing -> maybeBeginGame ready gs
        Just _  -> Nothing -- return $ (\rs -> gs & gameRound .~ Just rs) <$> newRiichiState

-- | If appropriate, begin the game
maybeBeginGame :: (p -> Bool) -> GameState p -> Maybe (IO (GameState p))
maybeBeginGame ready gs = do
    guard . null $ gs^.gamePlayers^..each.filtered (not . ready)
    return $ (\rs -> gs & gameRound .~ Just rs) <$> newRiichiState

-- ** Modify

-- | Try putting the given client to an empty player seat. Returns Nothing
-- if the game is already full.
addClient :: Eq playerID => playerID -> (playerID -> Bool) -> GameState playerID -> Maybe (GameState playerID)
addClient client f = uncurry (flip (<$)) . mapAccumLOf (gamePlayers.traversed) go Nothing
    where
        go s c | isNothing s && f c = (Just (), client)
               | otherwise          = (s, c)

removeClient :: Eq playerID => playerID -> GameState playerID -> Maybe (GameState playerID)
removeClient client gs = do
    p <- clientToPlayer client gs
    return $ (gamePlayers.at p .~ Nothing) gs

-- ** Read

playerToClient :: GameState playerID -> Player -> Maybe playerID
playerToClient gs p = gs^.gamePlayers.at p

clientToPlayer :: Eq playerID => playerID -> GameState playerID -> Maybe Player
clientToPlayer c gs = gs^.gamePlayers & ifind (\_ x -> x == c) <&> view _1
