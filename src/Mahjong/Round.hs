------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Round
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Round where

------------------------------------------------------------------------------
import           Mahjong.Tiles
import           Mahjong.Hand
import           Mahjong.Hand.Mentsu
import           Mahjong.State

------------------------------------------------------------------------------
import qualified Data.Map as Map
import           Data.Maybe (fromJust)

-- * RoundM

-- | Context of game and deal flow.
--
-- Note that RiichiSecret is freely modifiable as a state, but
-- @RiichiPublic@ is read-only. Modifications to the public part must
-- be encoded in RoundEvents. This way it is trivial to keep clients'
-- public states in sync with minimum bandwidth.
type RoundM m =
    ( MonadReader RiichiPublic m
    , MonadState RiichiSecret m
    , MonadWriter [GameEvent] m
    , MonadError Text m
    , Functor m
    , Applicative m
    , Monad m
    )

-- * Logic

startRound :: RoundM m => m ()
startRound = do
    view riichiPlayers >>= mapM (getPlayerState . view _1)
                       >>= tell . map RoundPrivateStarts
    view riichiTurn >>= startTurnOfPlayer

startTurnOfPlayer :: RoundM m => Player -> m ()
startTurnOfPlayer tp = do
    tellEvent $ RoundTurnBegins tp
    runTurn tp $ TurnTileDraw False Nothing

-- | Attempt to run a @TurnAction@ as the given user. Fails if it is not
-- his turn.
runTurn :: RoundM m => Player -> TurnAction -> m ()
runTurn p ta = do
    view riichiTurn >>= (`when` throwError "Not your turn") . (/= p)
    publishTurnAction p ta
    handOf' p >>= run >>= updateHand p
    where
        run = case ta of
            TurnTileDiscard True  tile -> discardRiichi tile -- TODO move to next player
            TurnTileDiscard False tile -> discard tile
            TurnAnkan tile             -> ankanOn tile
            TurnTileDraw False _       -> drawWall
            TurnTileDraw True  _       -> drawDeadWall

runShout :: RoundM m => Shout -> Player -> m ()
runShout shout shouter = do
    turn      <- view riichiTurn
    (m, hand) <- shoutFromHand shout =<< handOf' turn
    updateHand turn hand

    handOf' shouter >>= meldTo shout m >>= updateHand shouter
    tellEvent $ RoundTurnShouted shouter shout

-- | If win(s) were declared, wall was exhausted or four kans were declared
-- (and TODO declarer is not in the yakuman tenpai): return "RoundResults".
--
-- Otherwise the turn is passed to next player as if the player in turn
-- discarded previously.
advanceAfterDiscard :: RoundM m => m (Maybe RoundResults)
advanceAfterDiscard = do
    tilesLeft <- view riichiWallTilesLeft
    dora      <- view riichiDora
    case () of
        _ | tilesLeft == 0 || length dora == 5 -> Just <$> roundEndedDraw
          | otherwise                          -> do
                startTurnOfPlayer . nextPlayer =<< view riichiTurn
                return Nothing

-- ** Auxilary

advanceAuto :: RiichiState -> TurnAction
advanceAuto = TurnTileDiscard False . handAutoDiscard . fromJust <$>
    (view (riichiPublic.riichiTurn) >>= view . (riichiSecret . ) . handOf)

nextPlayer :: Player -> Player
nextPlayer = toEnum . (`mod` 4) . (+ 1) . fromEnum

handOf :: Player -> Lens RiichiSecret RiichiSecret (Maybe Hand) (Maybe Hand)
handOf player = riichiHands.at player

handOf' :: RoundM m => Player -> m Hand
handOf' player = use (handOf player) >>= maybe (throwError "Player not found") return

-- | Build the player's "@GamePlayer@" record, or the state of the game as
-- seen by the player (hide "RiichiSecret" but show the player's own hand).
getPlayerState :: RoundM m => Player -> m GamePlayer
getPlayerState p = GamePlayer
    <$> pure p
    <*> view id
    <*> use (riichiHands.to (map _handPublic))
    <*> view riichiPlayers
    <*> use (riichiHands.at p.to fromJust)

tellPlayerState :: RoundM m => Player -> m ()
tellPlayerState = getPlayerState >=> tell . return . RoundPrivateStarts

applyGameEvents' :: [GameEvent] -> RiichiPublic -> RiichiPublic
applyGameEvents' evs rp = _playerPublic $ applyGameEvents evs $ GamePlayer
    (error "Not accessed")
    rp mempty mempty $ Hand [] Nothing Nothing $ HandPublic [] [(error "N/A", Nothing)] False Nothing

applyGameEvents :: [GameEvent] -> GamePlayer -> GamePlayer
applyGameEvents evs gp = foldr applyGameEvent gp evs

applyGameEvent :: GameEvent -> GamePlayer -> GamePlayer
applyGameEvent ev = case ev of
    RoundTurnBegins p        -> playerPublic.riichiTurn .~ p
    RoundTurnAction p ta     -> applyTurnAction p ta
    RoundTurnShouted p shout ->
        over (playerPublicHands.at p._Just.handOpen) (|> fromShout shout)
        . set (playerPublic.riichiTurn) p
        . set (playerPublicHands.at (shoutedFrom shout)._Just.handDiscards._last._2) (Just p)
    RoundHandChanged p hp    -> playerPublicHands.at p._Just .~ hp
    RoundEnded how           -> playerPublic.riichiResults .~ Just how
    RoundPrivateChange _ h   -> playerMyHand .~ h
    RoundPrivateStarts gp    -> const gp
    RoundPrivateWaitForShout _ -> id

applyTurnAction :: Player -> TurnAction -> GamePlayer -> GamePlayer
applyTurnAction p ta = case ta of
    TurnTileDiscard riichi tile ->
        over (playerPublicHands.at p._Just.handDiscards) (|> (tile, Nothing))
        . set (playerPublicHands.at p._Just.handRiichi) riichi
    TurnTileDraw _ _  -> playerPublic.riichiWallTilesLeft -~ 1
    TurnAnkan tile    -> over (playerPublicHands.at p._Just.handOpen) (|> kantsu tile)

-- * Operations

-- | Set the hand of player
updateHand :: RoundM m => Player -> Hand -> m ()
updateHand p new = do
    old <- handOf' p
    handOf p ?= new
    when (old /= new) $ tell [ RoundPrivateChange p new ]
    when (_handPublic old /= _handPublic new) $ tell [RoundHandChanged p $ _handPublic new]

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- RoundEvent.
publishTurnAction :: RoundM m => Player -> TurnAction -> m ()
publishTurnAction player ra = tell $ case ra of
    TurnTileDraw b _ -> [ RoundTurnAction player $ TurnTileDraw b Nothing ]
    _                -> [ RoundTurnAction player ra ]

tellEvent :: RoundM m => GameEvent -> m ()
tellEvent = tell . return

-- ** Get results

roundEndedDraw :: RoundM m => m RoundResults
roundEndedDraw =
    roundEndsWith $ RoundDraw [] [] -- TODO tenpai players

roundEndsWith :: RoundM m => RoundResults -> m RoundResults
roundEndsWith results = do
    tell [RoundEnded results]
    return results

-- ** Player in turn

drawWall :: RoundM m => Hand -> m Hand
drawWall hand = do
    wall <- use riichiWall
    case wall of
        (x:xs) -> do riichiWall .= xs
                     return $ set handPick (Just x) hand
        _ -> throwError "No tiles left"

drawDeadWall :: RoundM m => Hand -> m Hand
drawDeadWall hand = preuse (riichiWall._Snoc)
    >>= maybe (throwError "No tiles in wall!") (draw . fst)
    where
        draw wall = do
            riichiWall .= wall
            Just (dt, wanpai) <- preuse (riichiWanpai._Cons)
            riichiWanpai .= wanpai
            return $ handPick .~ Just dt $ hand

-- | Set riichiWaitShoutsFrom to players who could shout the discard.
endTurn :: RoundM m => Tile -> m ()
endTurn dt = do
    hands <- Map.filter (not . null . shoutsOn dt) <$> use riichiHands
    riichiWaitShoutsFrom .= Map.keys hands
