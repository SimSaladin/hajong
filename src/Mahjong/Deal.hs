{-# LANGUAGE TupleSections, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Deal
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Mahjong.Deal where

------------------------------------------------------------------------------
import           Mahjong.Tiles
import           Mahjong.Hand
import           Mahjong.Hand.Algo (tenpai)
import           Mahjong.State

------------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Data.List as L (delete)
import           Data.Maybe (fromJust)

-- | Context of game and deal flow.
--
-- Note that Deal is freely modifiable as a state, but
-- @Deal@ is read-only. Modifications to the public part must
-- be encoded in DealEvents. This way it is trivial to keep clients'
-- public states in sync with minimum bandwidth.
type DealM m =
    ( MonadReader Deal m
    , MonadState Deal m
    , MonadWriter [GameEvent] m
    , MonadError Text m
    , Functor m
    , Applicative m
    , Monad m
    )

----------------------------------------------------------------------------------------

-- * Logic

startDeal :: DealM m => m ()
startDeal = do
    view pPlayers >>= imapM_ (\p v -> buildPlayerState p v >>= tellEvent . DealPrivateStarts)
    view pTurn >>= startTurn

turnWaiting :: DealM m => Int -> m ()
turnWaiting n = do
    tp <- view pTurn >>= kazeToPlayer
    tellEvent $ DealPrivateWaitForTurnAction tp n

startTurn :: DealM m => Kaze -> m ()
startTurn = tellEvent . DealTurnBegins

advanceTurn :: DealM m => m ()
advanceTurn = startTurn . nextKaze =<< view pTurn

-- | If win(s) were declared, wall was exhausted or four kans were declared
-- (and TODO declarer is not in the yakuman tenpai): return "DealResults".
--
-- Otherwise the turn is passed to next player as if the player in turn
-- discarded previously.
advanceAfterDiscard :: DealM m => m (Maybe DealResults)
advanceAfterDiscard = do
    tilesLeft <- view pWallTilesLeft
    dora      <- view pDora
    case () of
        _ | tilesLeft == 0 || length dora == 5 -> Just <$> endDraw
          | otherwise                          -> advanceTurn >> return Nothing

-- | @advanceWithShout shout shouter@
advanceWithShout :: DealM m => Shout -> Player -> m (Maybe DealResults)
advanceWithShout shout sp = do
    sk <- playerToKaze sp
    tk <- view pTurn
    tp <- kazeToPlayer tk
    (m, hand) <- shoutFromHand sk shout =<< handOf' tk
    updateHand tk hand
    handOf' sk >>= meldTo shout m >>= updateHand sk
    tellEvent $ DealTurnShouted sk shout
    if shoutKind shout == Ron
        then do
            win <- toWinner sp
            Just <$> roundEnds (DealRon [win] [(tp, win^._2.vhValue.vaValue)])
        else startTurn sk >> return Nothing

-- ** Actions

-- *** Run

-- | Attempt to run a @TurnAction@ as the given player. Fails if it is not
-- his turn or the action would be invalid.
runTurn :: DealM m => Player -> TurnAction -> m (Maybe DealResults)
runTurn tp ta = flip runTurn' ta =<< playerToKaze tp

-- | Like 'runTurn' but takes a kaze.
runTurn' :: DealM m => Kaze -> TurnAction -> m (Maybe DealResults)
runTurn' pk ta = do
    view pTurn >>= (`when` throwError "Not your turn") . (/= pk)
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
            TurnShouminkan tile        -> shouminkanOn tile h

-- *** Player in turn

drawWall :: DealM m => Hand -> m Hand
drawWall hand = do
    unless (canDraw hand) (throwError "Cannot draw from wall")
    wall <- use sWall
    case wall of
        (x:xs) -> do sWall .= xs
                     return $ handPick .~ Just x $ hand
        _ -> throwError "No tiles left"

drawDeadWall :: DealM m => Hand -> m Hand
drawDeadWall hand = preuse (sWall._Snoc)
    >>= maybe (throwError "No tiles in wall!") (draw . fst)
    where
        draw wall = do
            unless (hand^.handPublic.handDrawWanpai) (throwError "Cannot draw from wanpai")
            sWall .= wall
            Just (dt, wanpai) <- preuse (sWanpai._Cons)
            sWanpai .= wanpai
            return $ handPick .~ Just dt $ handPublic.handDrawWanpai .~ False $ hand

-- | Set sWaitingShouts to players who could shout the discard.
endTurn :: DealM m => Tile -> m ()
endTurn dt = do
    shouts <- filterCouldShout dt <$> view pTurn <*> use sHands
    sWaitingShouts .= shouts

-- *** Automatic actions

autoDiscard :: DealM m => m ()
autoDiscard = do
    tk <- view pTurn
    void $ runTurn' tk . TurnTileDiscard False =<< handAutoDiscard =<< handOf' tk

autoDraw, autoDrawWanpai :: DealM m => m ()
autoDraw = void . flip runTurn' (TurnTileDraw False Nothing) =<< view pTurn
autoDrawWanpai = void . flip runTurn' (TurnTileDraw True Nothing) =<< view pTurn

-- ** Results

endDraw :: DealM m => m DealResults
endDraw = do
    hands <- use sHands
    let (tenpaiPlayers, nootenPlayers) = both.each %~ fst $ partition (tenpai . snd) $ itoList hands
    res <- DealDraw <$> mapM kazeToPlayer tenpaiPlayers
                    <*> mapM (kazeToPlayer >=> return . (, 1000)) nootenPlayers
    roundEnds res

endTsumo :: DealM m => m DealResults
endTsumo = do
    tk      <- view pTurn
    players <- view pPlayers <&> map fst . itoList
    tp      <- kazeToPlayer tk
    oja     <- view pOja
    win     <- toWinner tp
    roundEnds $ DealTsumo [win] (tsumoPayers oja (win^._2.vhValue.vaValue) $ L.delete tp players)

roundEnds :: DealM m => DealResults -> m DealResults
roundEnds results = tell [DealEnded results] >> return results

toWinner :: DealM m => Player -> m Winner
toWinner p = do
    pk <- playerToKaze p
    rk <- view pRound
    h  <- handOf' pk
    return (p, valueHand h rk pk)

tsumoPayers :: Player -> Points -> [Player] -> [Payer]
tsumoPayers oja points payers
    | oja `elem` payers = map (, floor $ fromIntegral points / 3) payers
    | otherwise         = (oja, floor $ fromIntegral points /  2)
                        : map (, floor $ fromIntegral points / 4) (L.delete oja payers)

----------------------------------------------------------------------------------------

-- * Events

tellPlayerState :: DealM m => Player -> m ()
tellPlayerState p =
    view (pPlayers.at p)
    >>= maybe (throwError "tellPlayerState: player not found")
              (buildPlayerState p >=> tellEvent . DealPrivateStarts)

updatePlayerNick :: DealM m => Player -> Text -> m ()
updatePlayerNick p nick = playerToKaze p >>= \pk -> tellEvent (DealNick p pk nick)

-- | Build the player's "@GamePlayer@" record, or the state of the game as
-- seen by the player (hide "Deal" but show the player's own hand).
buildPlayerState :: DealM m => Player -> (Kaze, Points, Text) -> m GamePlayer
buildPlayerState p (k, _, name) = GamePlayer k p name
    <$> view id
    <*> use (sHands.to (map _handPublic))
    <*> use (sHands.at k.to fromJust)

-- | Set the hand of player
updateHand :: DealM m => Kaze -> Hand -> m ()
updateHand pk new = do
    old <- handOf' pk
    handOf pk ?= new

    when (old /= new) $ do
        pp <- kazeToPlayer pk
        tellEvent (DealPrivateChange pp new)

    when (_handPublic old /= _handPublic new)
        $ tellEvent (DealHandChanged pk $ _handPublic new)

tellEvent :: DealM m => GameEvent -> m ()
tellEvent ev = tell [ev]

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- DealEvent.
publishTurnAction :: DealM m => Kaze -> TurnAction -> m ()
publishTurnAction pk ra = tellEvent $ case ra of
    TurnTileDraw b _ -> DealTurnAction pk (TurnTileDraw b Nothing)
    _                -> DealTurnAction pk ra

getWaitingForShouts :: DealM m => m [(Player, Kaze, Shout)]
getWaitingForShouts = do

    let secs = 15 -- ^ TODO hard-coded limit

    allWaits <- use sWaitingShouts
    players  <- mapM (kazeToPlayer . fst) allWaits

    let res = zipWith (uncurry . (,,)) players allWaits
        out = map (DealPrivateWaitForShout <$> (^?!_head._1) <*> pure secs <*> (^..each._3))
            $ groupBy ((==) `on` view _1) res

    tell out
    return res

----------------------------------------------------------------------------------------

-- * Query info

kazeToPlayer :: DealM m => Kaze -> m Player
kazeToPlayer k = do
    mp <- view pPlayers <&> ifind (\_ x -> x^._1 == k)
    maybe (throwError "Player not found") (return . fst) mp

playerToKaze :: DealM m => Player -> m Kaze
playerToKaze p = do
    rp <- view pPlayers
    let mk = rp ^. at p
    maybe (throwError "Player not found") (return . (^._1)) mk

handOf' :: DealM m => Kaze -> m Hand
handOf' p = use (handOf p) >>= maybe (throwError "handOf': Player not found") return

----------------------------------------------------------------------------------------

-- * Functions

handOf :: Kaze -> Lens Deal Deal (Maybe Hand) (Maybe Hand)
handOf player = sHands.at player

-- | Advance the game to next deal, or end it.
nextDeal :: Deal -> IO (Either GameResults Deal)
nextDeal deal = case maybeGameResults deal of
    Just r  -> return (Left r)
    Nothing -> do
        let po      = deal^.pOja
            around  = deal^.pResults & goesAround po
            np      = deal^.pPlayers & (if around then each._1 %~ prevKaze else id)

            po_k                    = np^?!ix po._1
            Just (oja, (oja_k,_,_)) = np & ifind (\_ (k,_,_) -> k == po_k)

            go = set pPlayers np
               . set pTurn Ton
               . set pResults Nothing
               . set pOja oja
               . over pRound (if' (oja_k == Ton && around) nextKaze id)

        Right . go <$> dealTiles (logDeal deal)

  where
    goesAround po (Just DealDraw{..}) = not (null dTenpais) || notElem po dTenpais
    goesAround po (Just res)          = notElemOf (each._1) po (dWinners res)
    goesAround _ _ = error "nextDeal: game ended prematurely, this is not possible"

    logDeal = do
        k <- view pRound
        over pDeals $ \case
                        [] -> [(k, 1)]
                        xs@((k',n):_) | k == k'   -> (k, n + 1) : xs
                                      | otherwise -> (k, n) : xs

-- | Results are returned if west or higher round has just ended and
-- someone is winning (over 30000 points).
maybeGameResults :: Deal -> Maybe GameResults
maybeGameResults Deal{..} = do
    guard (_pRound >= Nan)
    guard (_pPlayers ^? at _pOja._Just._1 == Just Pei)
    guard (maximumOf (traversed._2) _pPlayers > Just 30000)
    return . finalPoints $ view _2 <$> _pPlayers

filterCouldShout :: Tile -- ^ Tile to shout
                 -> Kaze -- ^ Whose tile
                 -> Map Kaze Hand
                 -> [(Kaze, Shout)] -- ^ Sorted in correct precedence (highest priority first)
filterCouldShout dt np = sortBy (shoutPrecedence np) .
    concatMap flatten . Map.toList . Map.mapWithKey (shoutsOn np dt)
  where flatten (k, xs) = map (k,) xs

finalPoints :: Map Player Points -> GameResults
finalPoints xs =
    let ((winner, _, total), xs') = imapAccumR
            (\p (p',ps',tot) ps -> if' (ps > ps') ((p, ps,tot), go ps)
                                       ((p',ps',tot + go ps), go ps))
                                       (error "", 0, 0) xs
    in xs' & ix winner .~ (- total)
  where
    go x = case x `divMod` 1000 of
        (r, b) | r > 30000 && b > 0 -> r + 1 - 30
               | otherwise          -> r - 30

-- ** Client functions

applyGameEvents :: [GameEvent] -> GamePlayer -> GamePlayer
applyGameEvents evs gp = foldr applyGameEvent gp evs

applyGameEvent :: GameEvent -> GamePlayer -> GamePlayer
applyGameEvent ev = case ev of
    DealTurnBegins p        -> playerPublic.pTurn .~ p
    DealTurnAction p ta     -> applyTurnAction p ta
    DealTurnShouted p shout ->
        (playerPublicHands.at p._Just.handCalled %~ (|> fromShout shout)) .
        (playerPublic.pTurn .~ p) .
        (playerPublicHands.at (shoutFrom shout)._Just.handDiscards._last._2 .~ Just p)
    DealHandChanged p hp    -> playerPublicHands.at p._Just .~ hp
    DealEnded how           -> playerPublic.pResults .~ Just how
    DealPrivateChange _ h   -> playerMyHand .~ h
    DealPrivateStarts gp    -> const gp
    DealPrivateWaitForShout{} -> id
    DealPrivateWaitForTurnAction _ _ -> id
    DealNick p _ n -> playerPublic.pPlayers.ix p._3 .~ n

-- | This always applies the turn action assuming that it is legal.
applyTurnAction :: Kaze -> TurnAction -> GamePlayer -> GamePlayer
applyTurnAction p ta = case ta of
    TurnTileDiscard riichi tile -> playerPublicHands.at p._Just %~
        (handDiscards %~ (|> (tile, Nothing))) . (handRiichi .~ riichi)
    TurnTileDraw _ _     -> playerPublic.pWallTilesLeft -~ 1
    TurnAnkan tile       -> playerPublicHands.at p._Just.handCalled %~ (|> kantsu tile)
    _                    -> id

applyGameEvents' :: [GameEvent] -> Deal -> Deal
applyGameEvents' evs rp = _playerPublic $ applyGameEvents evs
    $ GamePlayer (error "Not accessed") (error "Not accessed") (error "Not accessed")
                 rp mempty hand
    where
        hand = Hand [] Nothing Nothing (HandPublic [] [(error "N/A", Nothing)] False False Nothing)

applyGameEvent' :: GameEvent -> Deal -> Deal
applyGameEvent' ev = applyGameEvents' [ev]
