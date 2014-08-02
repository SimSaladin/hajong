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

import ClassyPrelude
import Control.Lens
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.RWS
import qualified Data.Map as Map
import qualified Data.List as L
import Data.Maybe (fromJust)
import System.Random.Shuffle

import Hajong.Game.Types
import Hajong.Game.Hand
import Hajong.Game.Tiles

defaultPlayers :: [Player]
defaultPlayers = [Player Ton .. Player Pei]

-- * GameState

-- | Create a new GameState with the given label.
newGameState :: Text -> GameState a
newGameState name = GameState players name Nothing
    where
        players = zip3 defaultPlayers (repeat Nothing) (repeat 25000)

-- | Execute a round action in the "GameState".
gsRoundAction :: RoundM' r -> GameState p -> Either Text (r, RiichiSecret, [RoundEvent])
gsRoundAction m = maybe (Left "No active round!") run . _gameRound
    where run rs = runRWST m (_riichiPublic rs) (_riichiSecret rs)

-- | Return an IO action to create the first round if all player seats are
-- occupied.
gsMaybeFirstRound :: GameState a -> Maybe (IO (GameState a))
gsMaybeFirstRound gs = do
    guard (gs^.gamePlayers & find (isn't _Just.view _2) & isNothing)
    case _gameRound gs of
        Nothing -> Just $ (\rs -> gs & gameRound .~ Just rs) <$> newRiichiState
        Just _  -> Nothing

-- | Nothing if game full
gsAddPlayer :: Eq a => a -> GameState a -> Maybe (GameState a)
gsAddPlayer a gs = do
    p <- findFree gs
    return $ gs & over (gamePlayers.each)
        (if' <$> view (_1.to (==p)) <*> set _2 (Just a) <*> id)
    where
        findFree = fmap (view _1) . find (isn't _Just.view _2) . view gamePlayers

-- | Build the state visible to the player
gsPlayerLookup :: GameState id -> Player -> Maybe (GamePlayer id)
gsPlayerLookup game player = game^.gameRound^?_Just.to build
    where
        build = GamePlayer
            <$> pure player
            <*> view riichiPublic
            <*> view (riichiSecret.riichiHands.to (map _handPublic))
            <*> pure (game^.gamePlayers)
            <*> view (riichiSecret.riichiHands.at player.to fromJust)

-- * Rounds

-- | Given given @TurnAction@ as given player. If the player's hand was
-- modified the new hand is returned.
runTurn :: RoundM m => Player -> TurnAction -> m (Maybe Hand)
runTurn player action = do

    turnPlayer <- view riichiTurn
    when (turnPlayer /= player) $ fail "Not your turn"

    hand <- handOf' player

    newHand <- case action of
        TurnTileDiscard True  tile -> liftE (setRiichi tile hand) -- TODO move to next player
        TurnTileDiscard False tile -> liftE (discard tile hand)
        TurnAnkan tile            -> liftE (doAnkan tile hand)
        TurnTileDraw False _    -> drawWall hand
        TurnTileDraw True  _    -> drawDeadWall hand
        TurnShouted shout shouter ->
            liftE (doShout shout player hand) >>= runShout player shouter

    publishTurnAction player action

    handOf player ?= newHand
    when (_handPublic hand /= _handPublic newHand)
        $ tell [RoundPublicHand player $ _handPublic newHand]

    return $ if hand /= newHand then Just hand else Nothing

-- | Auxiliary function
runShout :: RoundM m
    => Player -- ^ Whose discard
    -> Player -- ^ Who shouted
    -> (Hand, Player -> Hand -> Maybe (Either () Mentsu))
    -> m Hand
runShout turnPlayer shouter (turnHand, f) = do
    hand <- handOf' shouter
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
    undefined -- TODO

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- RoundEvent.
publishTurnAction :: RoundM m => Player -> TurnAction -> m ()
publishTurnAction player ra = tell $ case ra of
    TurnTileDraw b _ -> [ RoundTurnAction player $ TurnTileDraw b Nothing ]
    _                -> [ RoundTurnAction player ra ]

-- | Set riichiWaitShoutsFrom to players who could shout the discard.
endTurn :: RoundM m => Tile -> m ()
endTurn dt = do
    hands <- Map.filter (couldShout dt) <$> use riichiHands
    unless (null hands) $ riichiWaitShoutsFrom .= Just (Map.keys hands)

-- | True if the tile could be shouted to the hand.
couldShout :: Tile -> Hand -> Bool
couldShout tile hand = True -- TODO

-- * Create round state

-- | Four-player riichi game
newPublic :: RiichiPublic
newPublic = RiichiPublic
    { _riichiDora          = []
    , _riichiWallTilesLeft = 0
    , _riichiRound         = Ton
    , _riichiDealer        = Player Ton
    , _riichiTurn          = Player Ton
    , _riichiPoints        = Map.fromList $ zip defaultPlayers (repeat 25000)
    , _riichiEvents        = []
    }

newSecret :: IO RiichiSecret
newSecret = liftM dealTiles $ shuffleM riichiTiles
    where
        dealTiles tiles = RiichiSecret
            { _riichiWall           = wall
            , _riichiWanpai         = wanpai
            , _riichiHands          = Map.fromList $ zip defaultPlayers (map initHand [h1, h2, h3, h4])
            , _riichiWaitShoutsFrom = Nothing
            } where
                (hands, xs)             = splitAt (13 * 4) tiles
                ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
                (wanpai, wall)          = splitAt 14 xs

-- | New state with first round ready to start. Convenient composite of
-- newPublic, newSecret and setSecret.
newRiichiState :: IO RiichiState
newRiichiState = liftM (`setSecret` newPublic) newSecret

setSecret :: RiichiSecret -> RiichiPublic -> RiichiState
setSecret secret public = RiichiState
    (secret & set riichiWanpai wanpai')
    (public & set riichiDora [dora] & set riichiWallTilesLeft (secret ^. riichiWall.to length))
    where
        (dora : wanpai') = secret ^. riichiWanpai

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound rs = do
    secret <- newSecret
    return $ setSecret secret $ (rs ^. riichiPublic) & set riichiTurn (rs ^. riichiPublic.riichiDealer)
