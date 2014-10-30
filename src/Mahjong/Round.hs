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
    view riichiPlayers >>= mapM buildPlayerState
                       >>= tell . map RoundPrivateStarts
    view riichiTurn >>= startTurn

turnWaiting :: RoundM m => Int -> m ()
turnWaiting n = do
    tp <- view riichiTurn >>= kazeToPlayer
    tellEvent $ RoundPrivateWaitForTurnAction tp n

kazeToPlayer :: RoundM m => Kaze -> m Player
kazeToPlayer k =
    maybe (throwError "Player not found") (return . (^._2)) . find (^._1.to (== k))
    =<< view riichiPlayers

playerToKaze :: RoundM m => Player -> m Kaze
playerToKaze p =
    maybe (throwError "Player not found") (return . (^._1)) . find (^._2.to (== p))
    =<< view riichiPlayers

startTurn :: RoundM m => Kaze -> m ()
startTurn = tellEvent . RoundTurnBegins

advanceTurn :: RoundM m => m ()
advanceTurn = startTurn . nextKaze =<< view riichiTurn

autoDiscard :: RoundM m => m ()
autoDiscard = do
    tp <- view riichiTurn
    hand <- handOf' tp
    runTurn' tp (TurnTileDiscard False (handAutoDiscard hand))

autoDraw :: RoundM m => m ()
autoDraw = flip runTurn' (TurnTileDraw False Nothing) =<< view riichiTurn

-- | Attempt to run a @TurnAction@ as the given user. Fails if it is not
-- his turn.
runTurn :: RoundM m => Player -> TurnAction -> m ()
runTurn pp ta = flip runTurn' ta =<< playerToKaze pp

runTurn' :: RoundM m => Kaze -> TurnAction -> m ()
runTurn' pk ta = do
    view riichiTurn >>= (`when` throwError "Not your turn") . (/= pk)
    publishTurnAction pk ta
    handOf' pk >>= run >>= updateHand pk
    where
        run = case ta of
            TurnTileDiscard True  tile -> discardRiichi tile -- TODO move to next player
            TurnTileDiscard False tile -> discard tile
            TurnAnkan tile             -> ankanOn tile
            TurnTileDraw False _       -> drawWall
            TurnTileDraw True  _       -> drawDeadWall

runShout :: RoundM m => Shout -> Player -> m ()
runShout shout sp = runShout' shout =<< playerToKaze sp

getWaitingForShouts :: RoundM m => m [(Kaze, Player)]
getWaitingForShouts = do
    xs <- use riichiWaitShoutsFrom
    ys <- mapM kazeToPlayer xs
    tell $ map (`RoundPrivateWaitForShout` 20) ys
    return $ zip xs ys

-- | @runShout shout shouter@
runShout' :: RoundM m => Shout -> Kaze -> m ()
runShout' shout sk = do
    turnKaze  <- view riichiTurn
    (m, hand) <- shoutFromHand shout =<< handOf' turnKaze
    updateHand turnKaze hand
    handOf' sk >>= meldTo shout m >>= updateHand sk
    tellEvent $ RoundTurnShouted sk shout

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
          | otherwise                          -> advanceTurn >> return Nothing

tellPlayerState :: RoundM m => Player -> m ()
tellPlayerState p =
    maybe (throwError "tellPlayerState: player not found")
          (buildPlayerState >=> tellEvent . RoundPrivateStarts)
          . find (^. _2.to (== p)) =<< view riichiPlayers

handOf' :: RoundM m => Kaze -> m Hand
handOf' p = use (handOf p) >>= maybe (throwError "handOf': Player not found") return

-- | Build the player's "@GamePlayer@" record, or the state of the game as
-- seen by the player (hide "RiichiSecret" but show the player's own hand).
buildPlayerState :: RoundM m => (Kaze, Player, a) -> m GamePlayer
buildPlayerState (p, name, _) = GamePlayer p name
    <$> view id
    <*> use (riichiHands.to (map _handPublic))
    <*> use (riichiHands.at p.to fromJust)

-- | Set the hand of player
updateHand :: RoundM m => Kaze -> Hand -> m ()
updateHand pk new = do
    old <- handOf' pk
    handOf pk ?= new

    when (old /= new) $ do
        pp <- kazeToPlayer pk
        tellEvent (RoundPrivateChange pp new)

    when (_handPublic old /= _handPublic new)
        $ tellEvent (RoundHandChanged pk $ _handPublic new)

-- ** Results

roundEndedDraw :: RoundM m => m RoundResults
roundEndedDraw =
    roundEndsWith $ RoundDraw [] [] -- TODO tenpai players

roundEndsWith :: RoundM m => RoundResults -> m RoundResults
roundEndsWith results = do
    tell [RoundEnded results]
    return results

-- ** Player actions

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

-- ** Helpers

tellEvent :: RoundM m => GameEvent -> m ()
tellEvent ev = tell [ev]

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- RoundEvent.
publishTurnAction :: RoundM m => Kaze -> TurnAction -> m ()
publishTurnAction pk ra = tellEvent $ case ra of
    TurnTileDraw b _ -> RoundTurnAction pk (TurnTileDraw b Nothing)
    _                -> RoundTurnAction pk ra

-- * Functions

handOf :: Kaze -> Lens RiichiSecret RiichiSecret (Maybe Hand) (Maybe Hand)
handOf player = riichiHands.at player

-- | Next kaze or wrap back to Ton
nextKaze :: Kaze -> Kaze
nextKaze = toEnum . (`mod` 4) . (+ 1) . fromEnum

playersNextRound :: [(Kaze, Player, a)] -> [(Kaze, Player, a)]
playersNextRound xs = let (ks, ps', as') = unzip3 xs
                          Just (ps, p) = unsnoc ps'
                          Just (as, a) = unsnoc as'
                          in zip3 ks (p : ps) (a : as)

-- | Advance the game to next round
--
-- TODO: we do not count han-chans
nextRound :: RiichiState -> IO RiichiState
nextRound rs = do
    secret <- newSecret
    let newPlayers = rs^.riichiPublic.riichiPlayers & playersNextRound
        newOja     = headEx newPlayers ^. _2
        newRound   = if newOja == rs^.riichiPublic.riichiFirstOja
                         then nextKaze
                         else id
    return
        $ setSecret secret
        $ riichiPlayers .~ newPlayers
        $ riichiRound %~ newRound
        $ riichiOja .~ newOja
        $ riichiTurn .~ Ton
        $ riichiResults .~ Nothing
        $ rs ^. riichiPublic

-- ** Applying GameEvents

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
    RoundPrivateWaitForShout _ _ -> id
    RoundPrivateWaitForTurnAction _ _ -> id

applyTurnAction :: Kaze -> TurnAction -> GamePlayer -> GamePlayer
applyTurnAction p ta = case ta of
    TurnTileDiscard riichi tile ->
        over (playerPublicHands.at p._Just.handDiscards) (|> (tile, Nothing))
        . set (playerPublicHands.at p._Just.handRiichi) riichi
    TurnTileDraw _ _ -> playerPublic.riichiWallTilesLeft -~ 1
    TurnAnkan tile   -> over (playerPublicHands.at p._Just.handOpen) (|> kantsu tile)

applyGameEvents' :: [GameEvent] -> RiichiPublic -> RiichiPublic
applyGameEvents' evs rp = _playerPublic $ applyGameEvents evs
    $ GamePlayer (error "Not accessed") (error "_playerPlayer is not accessed") rp mempty hand
    where
        hand = Hand [] Nothing Nothing $ HandPublic [] [(error "N/A", Nothing)] False Nothing

applyGameEvent' :: GameEvent -> RiichiPublic -> RiichiPublic
applyGameEvent' ev = applyGameEvents' [ev]
