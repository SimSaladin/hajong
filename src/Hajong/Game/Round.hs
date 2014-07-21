------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Game.Round
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
-- Round logic
module Hajong.Game.Round where

import ClassyPrelude
import Control.Lens
import Control.Monad.RWS
import Control.Monad.Error
import qualified Data.List as L
import qualified Data.Map as Map
import System.Random.Shuffle

import Hajong.Game.Hand
import Hajong.Game.Tiles
import Hajong.Game.Yaku
import Hajong.Game.Types

-- | Context of game and deal flow.
type GameMonad m = ( MonadReader RiichiPublic m
                   , MonadState RiichiSecret m
                   , MonadWriter [RoundEvent] m
                   , MonadError Text m
                   )

defaultPlayers :: [Player]
defaultPlayers = [Player Ton .. Player Pei]

runTurn :: GameMonad m => Player -> TurnAction -> m (Maybe Hand)
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

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound state = do
    secret <- newSecret
    return $ setSecret secret $ set (riichiPublic.riichiTurn) (state ^. riichiPublic.riichiDealer) state

-- ** Game logic

processShout :: GameMonad m => Player -> Player -> (Hand, Player -> Hand -> Maybe (Either () Mentsu)) -> m Hand
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

drawWall :: GameMonad m => Hand -> m Hand
drawWall hand = do
    wall <- use riichiWall
    case wall of
        (x:xs) -> do riichiWall .= xs
                     return $ set handPick (Just x) hand
        _ -> throwError "No tiles left"

drawDeadWall :: GameMonad m => Hand -> m Hand
drawDeadWall hand = do
    wall <- use riichiWanpai
    undefined

-- * Round secret

setSecret :: RiichiSecret -> RiichiState -> RiichiState
setSecret secret = do
    set riichiSecret  $ set riichiWanpai wanpai' secret
    over riichiPublic $ do
        set riichiDora [dora]
        set riichiWallTilesLeft (secret ^. riichiWall.to length)
    where
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

-- * Functions

liftE :: GameMonad m => Either Text a -> m a
liftE = either throwError return

handOf' :: GameMonad m => Player -> m Hand
handOf' player = use (handOf player) >>= maybe (throwError "Player not found") return

