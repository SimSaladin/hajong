{-# LANGUAGE TupleSections #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Round
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
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

startTurn :: RoundM m => Kaze -> m ()
startTurn = tellEvent . RoundTurnBegins

advanceTurn :: RoundM m => m ()
advanceTurn = startTurn . nextKaze =<< view riichiTurn

autoDiscard :: RoundM m => m ()
autoDiscard = do
    tp <- view riichiTurn
    hand <- handOf' tp
    dt <- handAutoDiscard hand
    runTurn' tp (TurnTileDiscard False dt)

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
    handOf' pk >>= handAction >>= updateHand pk
    where
        handAction h = case ta of
            TurnTileDiscard True  tile -> discardRiichi tile h <* endTurn tile -- TODO move to next player
            TurnTileDiscard False tile -> discard tile h <* endTurn tile
            TurnAnkan tile             -> ankanOn tile h
            TurnTileDraw False _       -> drawWall h
            TurnTileDraw True  _       -> drawDeadWall h

-- | @runShout shout shouter@
runShout :: RoundM m => Shout -> Player -> m ()
runShout shout sp = do
    sk <- playerToKaze sp
    tk <- view riichiTurn
    (m, hand) <- shoutFromHand sk shout =<< handOf' tk
    updateHand tk hand
    handOf' sk >>= meldTo shout m >>= updateHand sk
    tellEvent $ RoundTurnShouted sk shout

getWaitingForShouts :: RoundM m => m [(Player, Kaze, Shout)]
getWaitingForShouts = do
    ws <- use riichiWaitShoutsFrom
    ps <- mapM (kazeToPlayer . fst) ws
    let res = zipWith (\p (k, s) -> (p, k, s)) ps ws
        out = map ( \xs@((p,_,_):_) -> RoundPrivateWaitForShout p 30 $ map (^._3) xs) $ groupBy ((==) `on` view _2) res
                                                               -- ^ TODO hard-coded limit
    tell out
    return res

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
    shouts <- filterCouldShout dt <$> view riichiTurn <*> use riichiHands
    riichiWaitShoutsFrom .= shouts

filterCouldShout :: Tile -- ^ Tile to shout
                 -> Kaze -- ^ Whose tile
                 -> Map Kaze Hand
                 -> [(Kaze, Shout)] -- ^ Sorted in correct precedence (highest priority first)
filterCouldShout dt np = sortBy (shoutPrecedence np) .
    concatMap flatten . Map.toList . Map.mapWithKey (shoutsOn np dt)
  where flatten (k, xs) = map (k,) xs

-- ** Helpers

tellEvent :: RoundM m => GameEvent -> m ()
tellEvent ev = tell [ev]

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- RoundEvent.
publishTurnAction :: RoundM m => Kaze -> TurnAction -> m ()
publishTurnAction pk ra = tellEvent $ case ra of
    TurnTileDraw b _ -> RoundTurnAction pk (TurnTileDraw b Nothing)
    _                -> RoundTurnAction pk ra

-- ** Query info

kazeToPlayer :: RoundM m => Kaze -> m Player
kazeToPlayer k =
    maybe (throwError "Player not found") (return . (^._2)) . find (^._1.to (== k))
    =<< view riichiPlayers

playerToKaze :: RoundM m => Player -> m Kaze
playerToKaze p =
    maybe (throwError "Player not found") (return . (^._1)) . find (^._2.to (== p))
    =<< view riichiPlayers

handOf' :: RoundM m => Kaze -> m Hand
handOf' p = use (handOf p) >>= maybe (throwError "handOf': Player not found") return

-- * Functions

handOf :: Kaze -> Lens RiichiSecret RiichiSecret (Maybe Hand) (Maybe Hand)
handOf player = riichiHands.at player

playersNextRound :: [(Kaze, Player, a)] -> [(Kaze, Player, a)]
playersNextRound xs = let (ks, ps', as') = unzip3 xs
                          Just (ps, p) = unsnoc ps'
                          Just (as, a) = unsnoc as'
                          in zip3 ks (p : ps) (a : as)

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound rs = over riichiPublic r . logRound . flip setSecret rs <$> newSecret
  where
    r = do np <- view $ riichiPlayers.to playersNextRound
           let no = headEx np ^. _2
           ar <- view $ riichiFirstOja.to (== no)
           (riichiPlayers .~ np)
               . (riichiRound %~ if' ar nextKaze id)
               . (riichiOja .~ no)
               . (riichiTurn .~ Ton)
               . (riichiResults .~ Nothing)
    logRound = do
        k <- view $ riichiPublic.riichiRound
        riichiRounds %~ (\case
                        [] -> [(k, 0)]
                        xs@((k',n):_) | k == k'   -> (k, n + 1) : xs
                                      | otherwise -> (k, n) : xs )

-- ** Client functions

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
    RoundPrivateWaitForShout{} -> id
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
        hand = Hand [] Nothing Nothing (HandPublic [] [(error "N/A", Nothing)] False)

applyGameEvent' :: GameEvent -> RiichiPublic -> RiichiPublic
applyGameEvent' ev = applyGameEvents' [ev]
