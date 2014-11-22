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
import           Mahjong.Hand.Algo (tenpai)
import           Mahjong.Hand.Mentsu
import           Mahjong.State

------------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Data.List as L (delete)
import           Data.Maybe (fromJust)

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

----------------------------------------------------------------------------------------

-- * Logic

startRound :: RoundM m => m ()
startRound = do
    view riichiPlayers >>= imapM_ (\p v -> buildPlayerState p v >>= tellEvent . RoundPrivateStarts)
    view riichiTurn >>= startTurn

turnWaiting :: RoundM m => Int -> m ()
turnWaiting n = do
    tp <- view riichiTurn >>= kazeToPlayer
    tellEvent $ RoundPrivateWaitForTurnAction tp n

startTurn :: RoundM m => Kaze -> m ()
startTurn = tellEvent . RoundTurnBegins

advanceTurn :: RoundM m => m ()
advanceTurn = startTurn . nextKaze =<< view riichiTurn

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
        _ | tilesLeft == 0 || length dora == 5 -> Just <$> endDraw
          | otherwise                          -> advanceTurn >> return Nothing

-- | @advanceWithShout shout shouter@
advanceWithShout :: RoundM m => Shout -> Player -> m (Maybe RoundResults)
advanceWithShout shout sp = do
    sk <- playerToKaze sp
    tk <- view riichiTurn
    tp <- kazeToPlayer tk
    (m, hand) <- shoutFromHand sk shout =<< handOf' tk
    updateHand tk hand
    handOf' sk >>= meldTo shout m >>= updateHand sk
    tellEvent $ RoundTurnShouted sk shout
    if shoutKind shout == Ron
        then Just <$> roundEnds (RoundRon [sp] [tp])
        else startTurn sk >> return Nothing

-- ** Actions

-- *** Run

-- | Attempt to run a @TurnAction@ as the given player. Fails if it is not
-- his turn or the action would be invalid.
runTurn :: RoundM m => Player -> TurnAction -> m (Maybe RoundResults)
runTurn tp ta = flip runTurn' ta =<< playerToKaze tp

-- | Like 'runTurn' but takes a kaze.
runTurn' :: RoundM m => Kaze -> TurnAction -> m (Maybe RoundResults)
runTurn' pk ta = do
    view riichiTurn >>= (`when` throwError "Not your turn") . (/= pk)
    publishTurnAction pk ta
    handOf' pk >>= handAction >>= updateHand pk
    if ta == TurnTsumo then Just <$> endTsumo else return Nothing
    where
        handAction h = case ta of
            TurnTileDiscard True  tile -> discardRiichi tile h <* endTurn tile
            TurnTileDiscard False tile -> discard tile h <* endTurn tile
            TurnAnkan tile             -> ankanOn tile h
            TurnTileDraw False _       -> drawWall h
            TurnTileDraw True  _       -> drawDeadWall h
            TurnTsumo                  -> handWin h

-- *** Player in turn

drawWall :: RoundM m => Hand -> m Hand
drawWall hand = do
    unless (canDraw hand) (throwError "Cannot draw from wall")
    wall <- use riichiWall
    case wall of
        (x:xs) -> do riichiWall .= xs
                     return $ handPick .~ Just x $ hand
        _ -> throwError "No tiles left"

drawDeadWall :: RoundM m => Hand -> m Hand
drawDeadWall hand = preuse (riichiWall._Snoc)
    >>= maybe (throwError "No tiles in wall!") (draw . fst)
    where
        draw wall = do
            unless (hand^.handPublic.handDrawWanpai) (throwError "Cannot draw from wanpai")
            riichiWall .= wall
            Just (dt, wanpai) <- preuse (riichiWanpai._Cons)
            riichiWanpai .= wanpai
            return $ handPick .~ Just dt $ handPublic.handDrawWanpai .~ False $ hand

-- | Set riichiWaitShoutsFrom to players who could shout the discard.
endTurn :: RoundM m => Tile -> m ()
endTurn dt = do
    shouts <- filterCouldShout dt <$> view riichiTurn <*> use riichiHands
    riichiWaitShoutsFrom .= shouts

-- *** Automatic actions

autoDiscard :: RoundM m => m ()
autoDiscard = do
    tk <- view riichiTurn
    void $ runTurn' tk . TurnTileDiscard False =<< handAutoDiscard =<< handOf' tk

autoDraw, autoDrawWanpai :: RoundM m => m ()
autoDraw = void . flip runTurn' (TurnTileDraw False Nothing) =<< view riichiTurn
autoDrawWanpai = void . flip runTurn' (TurnTileDraw True Nothing) =<< view riichiTurn

-- ** Results

endDraw :: RoundM m => m RoundResults
endDraw = do
    hands <- use riichiHands
    let (tenpaiPlayers, nootenPlayers) = both.each %~ fst $ partition (tenpai . snd) $ itoList hands
    res <- RoundDraw <$> mapM kazeToPlayer tenpaiPlayers <*> mapM kazeToPlayer nootenPlayers
    roundEnds res

endTsumo :: RoundM m => m RoundResults
endTsumo = do
    tk      <- view riichiTurn
    players <- view riichiPlayers <&> map fst . itoList
    tp      <- kazeToPlayer tk
    _hand   <- use (riichiHands.at tk)
    roundEnds $ RoundTsumo [tp] (L.delete tp players)

roundEnds :: RoundM m => RoundResults -> m RoundResults
roundEnds results = tell [RoundEnded results] >> return results

----------------------------------------------------------------------------------------

-- * Events

tellPlayerState :: RoundM m => Player -> m ()
tellPlayerState p =
    view (riichiPlayers.at p)
    >>= maybe (throwError "tellPlayerState: player not found")
              (buildPlayerState p >=> tellEvent . RoundPrivateStarts)

updatePlayerNick :: RoundM m => Player -> Text -> m ()
updatePlayerNick p nick = playerToKaze p >>= \pk -> tellEvent (RoundNick p pk nick)

-- | Build the player's "@GamePlayer@" record, or the state of the game as
-- seen by the player (hide "RiichiSecret" but show the player's own hand).
buildPlayerState :: RoundM m => Player -> (Kaze, Points, Text) -> m GamePlayer
buildPlayerState p (k, _, name) = GamePlayer k p name
    <$> view id
    <*> use (riichiHands.to (map _handPublic))
    <*> use (riichiHands.at k.to fromJust)

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

tellEvent :: RoundM m => GameEvent -> m ()
tellEvent ev = tell [ev]

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- RoundEvent.
publishTurnAction :: RoundM m => Kaze -> TurnAction -> m ()
publishTurnAction pk ra = tellEvent $ case ra of
    TurnTileDraw b _ -> RoundTurnAction pk (TurnTileDraw b Nothing)
    _                -> RoundTurnAction pk ra

----------------------------------------------------------------------------------------

-- * Query info

kazeToPlayer :: RoundM m => Kaze -> m Player
kazeToPlayer k = do
    mp <- view riichiPlayers <&> ifind (\_ x -> x^._1 == k)
    maybe (throwError "Player not found") (return . fst) mp

playerToKaze :: RoundM m => Player -> m Kaze
playerToKaze p = do
    rp <- view riichiPlayers
    let mk = rp ^. at p
    maybe (throwError "Player not found") (return . (^._1)) mk

handOf' :: RoundM m => Kaze -> m Hand
handOf' p = use (handOf p) >>= maybe (throwError "handOf': Player not found") return

getWaitingForShouts :: RoundM m => m [(Player, Kaze, Shout)]
getWaitingForShouts = do
    ws <- use riichiWaitShoutsFrom
    ps <- mapM (kazeToPlayer . fst) ws
    let res = zipWith (\p (k, s) -> (p, k, s)) ps ws
        out = map ( \xs@((p,_,_):_) -> RoundPrivateWaitForShout p 30 $ map (^._3) xs) $ groupBy ((==) `on` view _2) res
                                                               -- ^ TODO hard-coded limit
    tell out
    return res

----------------------------------------------------------------------------------------

-- * Functions

handOf :: Kaze -> Lens RiichiSecret RiichiSecret (Maybe Hand) (Maybe Hand)
handOf player = riichiHands.at player

playersNextRound :: [(Kaze, Player, a, b)] -> [(Kaze, Player, a, b)]
playersNextRound xs = let (ks, ps', as', bs') = unzip4 xs
                          Just (ps, p) = unsnoc ps'
                          Just (as, a) = unsnoc as'
                          Just (bs, b) = unsnoc bs'
                          in zip4 ks (p : ps) (a : as) (b : bs)

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound rs = over riichiPublic r . logRound . flip setSecret rs <$> newSecret
  where
    r = do np <- view riichiPlayers <&> each._1 %~ prevKaze
           po <- view riichiOja
        
           let po_k                    = np^?!ix po._1
           let Just (oja, (oja_k,_,_)) = np & ifind (\_ (k,_,_) -> k == po_k)

           (riichiPlayers .~ np)
               . (riichiTurn .~ Ton)
               . (riichiResults .~ Nothing)
               . (riichiOja .~ oja)
               . (riichiRound %~ if' (oja_k == Ton) nextKaze id)

    logRound = do
        k <- view $ riichiPublic.riichiRound
        riichiRounds %~ (\case
                        [] -> [(k, 0)]
                        xs@((k',n):_) | k == k'   -> (k, n + 1) : xs
                                      | otherwise -> (k, n) : xs )

filterCouldShout :: Tile -- ^ Tile to shout
                 -> Kaze -- ^ Whose tile
                 -> Map Kaze Hand
                 -> [(Kaze, Shout)] -- ^ Sorted in correct precedence (highest priority first)
filterCouldShout dt np = sortBy (shoutPrecedence np) .
    concatMap flatten . Map.toList . Map.mapWithKey (shoutsOn np dt)
  where flatten (k, xs) = map (k,) xs

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
    RoundNick p _ n -> playerPublic.riichiPlayers.ix p._3 .~ n

applyTurnAction :: Kaze -> TurnAction -> GamePlayer -> GamePlayer
applyTurnAction p ta = case ta of
    TurnTileDiscard riichi tile ->
        over (playerPublicHands.at p._Just.handDiscards) (|> (tile, Nothing))
        . set (playerPublicHands.at p._Just.handRiichi) riichi
    TurnTileDraw _ _ -> playerPublic.riichiWallTilesLeft -~ 1
    TurnAnkan tile   -> over (playerPublicHands.at p._Just.handOpen) (|> kantsu tile)
    TurnTsumo -> id -- XXX: is this function sensible?

applyGameEvents' :: [GameEvent] -> RiichiPublic -> RiichiPublic
applyGameEvents' evs rp = _playerPublic $ applyGameEvents evs
    $ GamePlayer (error "Not accessed") (error "Not accessed") (error "Not accessed")
                 rp mempty hand
    where
        hand = Hand [] Nothing Nothing (HandPublic [] [(error "N/A", Nothing)] False False)

applyGameEvent' :: GameEvent -> RiichiPublic -> RiichiPublic
applyGameEvent' ev = applyGameEvents' [ev]
