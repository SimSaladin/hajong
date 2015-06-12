{-# LANGUAGE TupleSections, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
------------------------------------------------------------------------------
-- |
-- Module         : Mahjong.Kyoku
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This module provides a mahjong state machine @Machine@ with state
-- @Kyoku@.
------------------------------------------------------------------------------
module Mahjong.Kyoku
    ( module Mahjong.Kyoku
    , module Mahjong.Kyoku.Internal
    ) where

------------------------------------------------------------------------------
import           Import
import           Mahjong.Tiles
import           Mahjong.Hand
import           Mahjong.Configuration
------------------------------------------------------------------------------
import           Mahjong.Kyoku.Internal
------------------------------------------------------------------------------
import qualified Data.Map as Map
import           Data.Monoid (Endo(..))
import qualified Data.List as L (delete, find)
------------------------------------------------------------------------------

-- | Context of game and deal flow.
--
-- @Kyoku@ data type is read-only. Modifications must be encoded in
-- 'GameEvent's. This way it is trivial to keep clients' public states in
-- sync.
type InKyoku m =
    ( MonadReader Kyoku m
    , MonadWriter [GameEvent] m
    , MonadError Text m
    , Functor m
    , Applicative m
    , Monad m
    )

handOf :: Kaze -> Lens Kyoku Kyoku (Maybe HandA) (Maybe HandA)
handOf pk = sHands.at pk

----------------------------------------------------------------------------------------

-- * Logic

-- TODO Something like this instead?
-- | Game automata
data Machine = NotBegun
             | CheckEndConditionsAfterDiscard
             | WaitingDraw Kaze Bool
             | WaitingDiscard Kaze
             | WaitingShouts (Maybe (Player, Shout)) [WaitShout]
             | Ended KyokuResults

data MachineInput = InpAuto
                  | InpTurnAction Kaze TurnAction
                  | InpShout Kaze Shout
                  | InpPass Kaze

-- | Remember to publish the turn action when successful
step :: InKyoku m => Machine -> MachineInput -> m Machine
step NotBegun InpAuto         = sendDealStarts >> waitForDraw -- startDeal

step (WaitingDraw pk wanpai) InpAuto = draw pk wanpai >> askForTurnAction 15 >> return (WaitingDiscard pk) -- TODO hard-coded
step (WaitingDraw pk wanpai) (InpTurnAction pk' (TurnTileDraw wanpai' _))
            | wanpai /= wanpai' = throwError "You are not supposed to draw there"
            | pk /= pk'         = throwError "Not your turn"
            | otherwise         = draw pk wanpai >> askForTurnAction 15 >> return (WaitingDiscard pk) -- TODO hard-coded

step (WaitingDiscard pk) InpAuto      = autoDiscard pk
step (WaitingDiscard pk) (InpTurnAction pk' ta)
            | pk /= pk'               = throwError "Not your turn"
            | TurnTileDiscard d <- ta = processDiscard pk d
            | TurnAnkan t <- ta       = handOf' pk >>= ankanOn t >>= updateHand pk >> return (WaitingDiscard pk)
            | TurnTsumo <- ta         = tsumo pk <&> Ended
            | TurnShouminkan t <- ta  = handOf' pk >>= shouminkanOn t >>= updateHand pk >> return (WaitingDraw pk True) -- TODO Wait shout robbing

step (WaitingShouts winning shouts) (InpShout pk shout) -- TODO Precedences not implemented
            | Just (_,_,_,xs) <- L.find (\(_,pk',_,_) -> pk' == pk) shouts
            , shout `elem` xs               = processShout pk shout >> return (WaitingDiscard pk)
            | otherwise                     = throwError "No such call is possible"
step (WaitingShouts Nothing shouts) InpAuto = return CheckEndConditionsAfterDiscard

step CheckEndConditionsAfterDiscard InpAuto = do
            tilesLeft <- view pWallTilesLeft
            dora      <- view pDora
            case () of
                _ | tilesLeft == 0   -> endDraw <&> Ended
                  | length dora == 5 -> error "Special condition handling not yet implemented" -- TODO Implement
                  | otherwise        -> advanceTurn <&> (`WaitingDraw` False)

-- ** Beginning

-- | Send the very first Kyoku events to everyone. Contains the game state.
sendDealStarts :: InKyoku m => m ()
sendDealStarts = do
    deal <- ask
    imapM_ (\p (pk,_,_) -> tellEvent . DealStarts p pk $ buildPlayerState deal pk) (deal^.pPlayers)

-- ** Drawing

-- | WaitingDraw, not wanpai
waitForDraw :: InKyoku m => m Machine
waitForDraw = do
    pk <- view pTurn
    h <- handOf' pk
    updateHand pk $ h & handState .~ DrawFromWall
    tellEvent $ DealTurnBegins pk
    return $ WaitingDraw pk False

draw :: InKyoku m => Kaze -> Bool -> m ()
draw pk wanpai = do updateHand pk =<< (if wanpai then drawDeadWall else drawWall) =<< handOf' pk

drawWall :: InKyoku m => HandA -> m HandA
drawWall hand = do
    preview (sWall._head) >>= maybe (throwError "Wall is empty") (`toHand` hand)

drawDeadWall :: InKyoku m => HandA -> m HandA
drawDeadWall hand = preview (sWall._last) >>= \case
    Nothing  -> throwError "Wall is empty"
    Just tow -> do
        t <- view (sWanpai.singular _head)
        tellEvent $ DealFlipDora t (Just tow)
        t `toHandWanpai` hand

-- ** Discarding

processDiscard :: InKyoku m => Kaze -> Discard -> m Machine
processDiscard pk d' = do
    let d = d' & dcTo .~ Nothing
    when (d^.dcRiichi) $ doRiichi pk
    updateHand pk =<< discard d =<< handOf' pk

    waits <- waitingShouts (d^.dcTile)
    tell $ map DealWaitForShout waits
    return $ if null waits
                 then CheckEndConditionsAfterDiscard
                 else WaitingShouts Nothing waits

doRiichi :: InKyoku m => Kaze -> m ()
doRiichi pk = do
    p <- kazeToPlayer pk
    np <- view $ pPlayers.at p.singular _Just._2.to (\a -> a - 1000)
    when (np < 0) $ throwError "Cannot riichi: not enough points"
    tell [DealRiichi pk, GamePoints p np]

autoDiscard :: InKyoku m => Kaze -> m Machine
autoDiscard tk = processDiscard tk =<< handAutoDiscard =<< handOf' tk

-- ** Turn-passing

advanceTurn :: InKyoku m => m Kaze
advanceTurn = do
    pk <- view pTurn <&> nextKaze
    tellEvent $ DealTurnBegins pk
    return pk

-- ** Waiting

-- | n seconds
askForTurnAction :: InKyoku m => Int -> m ()
askForTurnAction n = do
    tk <- view pTurn
    tp <- kazeToPlayer tk
    rt <- handOf' tk <&> handCanRiichiWith
    tellEvent $ DealWaitForTurnAction (tp, tk, n, rt)

-- | @processShout shouter shout@
processShout :: InKyoku m => Kaze -> Shout -> m Machine
processShout sk shout = do
    sp <- kazeToPlayer sk
    sh <- handOf' sk
    tk <- view pTurn
    tp <- kazeToPlayer tk
    th <- handOf' tk
    (m, th') <- shoutFromHand sk shout th
    sh' <- meldTo shout m sh
    updateHand sk sh' >> updateHand tk th'
    tellEvent $ DealTurnShouted sk shout

    if shoutKind shout == Ron
        then endRon sp tp <&> Ended
        else do tellEvent $ DealTurnBegins sk
                return (WaitingDiscard sk)

-- ** Ending

tsumo :: InKyoku m => Kaze -> m KyokuResults
tsumo pk = do
    handOf' pk >>= handWin Nothing >>= updateHand pk
    honba   <- view pHonba
    tk      <- view pTurn
    players <- view pPlayers <&> map fst . itoList
    tp      <- kazeToPlayer tk
    oja     <- view pOja
    win     <- toWinner tp
    dealEnds $ DealTsumo [win] (tsumoPayers honba oja (win^._3.vhValue.vaValue) $ L.delete tp players)

endDraw :: InKyoku m => m KyokuResults
endDraw = do
    hands <- view sHands
    let x@(tp, np) = both.each %~ fst $ partition (tenpai . snd) $ itoList hands
    let (r, p) | null tp || null np = (0, 0)
               | otherwise          = x & both %~ div 3000 . fromIntegral . length
    tp' <- mapM kazeToPlayer tp
    np' <- mapM kazeToPlayer np
    dealEnds $ DealDraw (map (,r) tp') (map (,p) np')

endRon :: InKyoku m => Player -> Player -> m KyokuResults
endRon sp tp = do
    h   <- view pHonba
    win <- toWinner sp
    oja <- view pOja
    let basic = win^._3.vhValue.vaValue
    dealEnds $ DealRon [win] [(tp, negate $ roundKyokuPoints $ basic * if' (tp == oja) 6 4 + h * 100)]

-- | Broadcast results with pass-through.
dealEnds :: InKyoku m => KyokuResults -> m KyokuResults
dealEnds results = do
    tell $ DealEnded results : payPoints results
    return results

-- ** Scoring

payPoints :: KyokuResults -> [GameEvent]
payPoints res = case res of
    DealAbort{}  -> []
    DealDraw{..} -> map g' dTenpais ++ map g dNooten
    _            -> map f (dWinners res) ++ map g (dPayers res)
  where f (p, v, _) = GamePoints p v
        g (p, v)    = GamePoints p (- v)
        g' (p, v)   = GamePoints p v

-- |
-- >>> roundKyokuPoints 150
-- 200
--
-- >>> roundKyokuPoints 500
-- 500
roundKyokuPoints :: Points -> Points
roundKyokuPoints x = case x `divMod` 100 of
    (a, b) | b > 0     -> (a + 1) * 100
           | otherwise -> a * 100

-- |
-- >>> tsumoPayers 0 (Player 0) 320 [Player 1, Player 2, Player 3]
-- [(Player 1,-700),(Player 2,-700),(Player 3,-700)]
tsumoPayers :: Int -> Player -> Points -> [Player] -> [Payer]
tsumoPayers honba oja basic payers
    | oja `elem` payers = map (\p -> (p, negate $ roundKyokuPoints $ basic * if' (p == oja) 2 1 + honba * 100)) payers
    | otherwise         = map (        , negate $ roundKyokuPoints $ basic * 2                  + honba * 100) payers

toWinner :: InKyoku m => Player -> m Winner
toWinner p = do
    pk <- playerToKaze p
    h  <- handOf' pk
    d  <- ask
    let vh  = valueHand pk h d
    return (p, roundKyokuPoints $ if' (p == _pOja d) 6 4 * (vh^.vhValue.vaValue) + _pHonba d * 300, vh)

-- * Final scoring

-- |
-- >>> finalPoints (Map.fromList [(Player 0,25000), (Player 1,25000), (Player 2,25000), (Player 3,25000)])
-- fromList [(Player 0,35),(Player 1,5),(Player 2,-15),(Player 3,-25)]
--
-- >>> finalPoints (Map.fromList [(Player 0, 35700), (Player 1, 32400), (Player 2, 22200), (Player 3, 9700)])
-- fromList [(Player 0,46),(Player 1,12),(Player 2,-18),(Player 3,-40)]
finalPoints :: PointsStatus -> FinalPoints
finalPoints xs = final & _head._2 -~ sumOf (each._2) final -- fix sum to 0
                       & Map.fromList & FinalPoints
    where
        target = 30000              -- TODO configurable target score
        oka    = 5000 * 4           -- TODO could be something else
        uma    = [20, 10, -10, -20] -- TODO This too
        final  = Map.toList xs & sortBy (flip $ comparing snd)
                               & each._2 -~ target
                               & _head._2 +~ oka
                               & each._2 %~ roundFinalPoints
                               & zipWith (\s (p, s') -> (p, s + s')) uma

-- |
-- >>> roundFinalPoints 15000
-- 15
-- >>> roundFinalPoints (-5000)
-- -5
roundFinalPoints :: Int -> Int
roundFinalPoints x = case x `divMod` 1000 of
    (r, b) | b >= 500 -> r + 1
           | otherwise -> r

----------------------------------------------------------------------------------------

-- * Events

tellPlayerState :: InKyoku m => Player -> m ()
tellPlayerState p = do
    pk <- playerToKaze p
    deal <- ask
    tellEvent . DealStarts p pk $ buildPlayerState deal pk

updatePlayerNick :: InKyoku m => Player -> Text -> m ()
updatePlayerNick p nick = playerToKaze p >>= \pk -> tellEvent (DealNick p pk nick)

-- | Build the player's "@GamePlayer@" record, or the state of the game as
-- seen by the player (hide "Deal" but show the player's own hand).
buildPlayerState :: Kyoku -> Kaze -> AsPlayer
buildPlayerState deal pk = flip appEndo (deal { _sHands = imap (\k -> if pk == k then convertHand else maskPublicHand) (_sHands deal) }) $ mconcat
    [ Endo $ sEvents .~ []
    , Endo $ sWall .~ []
    , Endo $ sWanpai .~ []
    , Endo $ sWaiting %~ mwaiting
    ] where
        mwaiting :: Maybe Waiting -> Maybe Waiting
        mwaiting x
            | x ^? _Just._Left._2 == Just pk = x
            | otherwise = case x ^.. _Just._Right.folded.filtered (^._2.to(==pk)) of
                        [] -> Nothing
                        xs -> Just (Right xs)

-- | Set the hand of player
updateHand :: InKyoku m => Kaze -> HandA -> m ()
updateHand pk = updateHand' pk (return ())

updateHand' :: InKyoku m => Kaze -> m a -> HandA -> m a
updateHand' pk ma new = do
    p   <- kazeToPlayer pk
    old <- handOf' pk
    when (old /= new)
        $ tellEvent (DealPrivateHandChanged p pk new)
    when (maskPublicHand old /= maskPublicHand new)
        $ tellEvent (DealPublicHandChanged pk $ maskPublicHand new)
    local (sHands.at pk .~ Just new) ma

tellEvent :: InKyoku m => GameEvent -> m ()
tellEvent ev = tell [ev]

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- DealEvent.
publishTurnAction :: InKyoku m => Kaze -> TurnAction -> m ()
publishTurnAction pk ra = tellEvent $ case ra of
    TurnTileDraw b _ -> DealTurnAction pk (TurnTileDraw b Nothing)
    _                -> DealTurnAction pk ra

----------------------------------------------------------------------------------------

-- * Query info

kazeToPlayer :: InKyoku m => Kaze -> m Player
kazeToPlayer k = do
    mp <- view pPlayers <&> ifind (\_ x -> x^._1 == k)
    maybe (throwError "Player not found") (return . fst) mp

playerToKaze :: InKyoku m => Player -> m Kaze
playerToKaze p = do
    rp <- view pPlayers
    let mk = rp ^. at p
    maybe (throwError "Player not found") (return . (^._1)) mk

handOf' :: InKyoku m => Kaze -> m HandA
handOf' p = view (handOf p) >>= maybe (throwError "handOf': Player not found") return

----------------------------------------------------------------------------------------

-- * Kyoku ends

-- | Advance the game to next deal, or end it.
nextDeal :: Kyoku -> IO (Either FinalPoints Kyoku)
nextDeal deal = case maybeGameResults deal of
    Just r  -> return (Left r)
    Nothing -> do
        let po      = deal^.pOja
            around  = deal^.pResults & goesAround po
            honba   = deal^.pResults & newHonba po
            np      = deal^.pPlayers & (if around then each._1 %~ prevKaze else id)

            po_k                    = np^?!ix po._1
            Just (oja, (oja_k,_,_)) = np & ifind (\_ (k,_,_) -> k == po_k)

            go = set pPlayers np
               . set pTurn Ton
               . set pResults Nothing
               . set pOja oja
               . over pHonba honba
               . over pRound (if' (oja_k == Shaa && around) nextKaze id)

            in Right . go <$> dealTiles (logDeal deal)

  where
    goesAround po (Just DealDraw{..}) = not (null dTenpais) || all ((/= po) . fst) dTenpais
    goesAround po (Just res)          = notElemOf (each._1) po (dWinners res)
    goesAround _ _ = error "nextDeal: game ended prematurely, this is not possible"

    newHonba _ (Just DealDraw{})  = (+1)
    newHonba _ (Just DealAbort{}) = (+1)
    newHonba po (Just x) | notElemOf (each._1) po (dWinners x) = const 0
                         | otherwise                           = (+1)
    newHonba _ _ = error "newHonba: game ended prematurely, this is not possible"
    logDeal = do
        k <- view pRound
        over pDeals $ \case
                        [] -> [(k, 1)]
                        xs@((k',n):_) | k == k'   -> (k, n + 1) : xs
                                      | otherwise -> (k, n) : xs

-- | Results are returned if west or higher round has just ended and
-- someone is winning (over 30000 points).
maybeGameResults :: Kyoku -> Maybe FinalPoints
maybeGameResults Kyoku{..}
    | minimumOf (traversed._2) _pPlayers < Just 0 = score
    | otherwise = do
        guard (_pRound >= Nan)
        guard (_pPlayers ^? at _pOja._Just._1 == Just Nan)
        guard (maximumOf (traversed._2) _pPlayers > Just 30000)
        score
  where
    score = return . finalPoints $ view _2 <$> _pPlayers

-- * Apply events

dealGameEvent :: GameEvent -> Kyoku -> Kyoku
dealGameEvent ev = appEndo . mconcat $ case ev of

    DealTurnBegins p ->
        [ Endo $ pTurn .~ p
        , Endo $ sWaiting .~ Nothing ]

    DealTurnAction p ta ->
        [ dealTurnAction p ta ]

    DealTurnShouted p _shout ->
        [ Endo $ pTurn .~ p
         {-, Endo $ sHands.ix p %~
               ( handPublic.handCalled %~ (|> fromShout shout)
               . handConcealed %~ (L.\\ shoutTo shout)
               . if' (shoutKind shout == Kan) (handPublic.handDrawWanpai .~ True) id
               )
        , Endo $ sHands.ix (shoutFrom shout)
               . handPublic.handDiscards._last.dcTo .~ Just p -}
        ]

    -- TODO get rid these in favor of finer control
    DealPublicHandChanged pk hp -> [ ]
    --     [ Endo $ sHands.ix pk.handPublic .~ hp ]

    DealPrivateHandChanged _ pk h ->
        [ Endo $ sHands.ix pk .~ h ]

    DealEnded how ->
        [ Endo $ pResults .~ Just how ]

    DealNick p _ nick ->
        [ Endo $ pPlayers.ix p._3 .~ nick ]

    -- player-private
    DealStarts _ _ new -> [ ]
        -- | null (_sWanpai new) -> [] -- TODO checks if player; the whole function is to work on server for now
        -- | otherwise           -> [ Endo $ const new ]

    DealWaitForShout ws ->
        [ Endo $ sWaiting %~ Just . Right . maybe [ws] (either (const [ws]) (|> ws)) ]

    DealWaitForTurnAction wt ->
        [ Endo $ sWaiting .~ Just (Left wt) ]

    DealRiichi pk ->
        [ Endo $ pRiichi +~ 1000 ]

    DealFlipDora td mtw ->
        [ Endo $ pDora %~ (|> td)
        , Endo $ maybe id (over sWanpai . flip snoc) mtw ]

    GamePoints pk ps ->
        [ Endo $ pPlayers.ix pk._2 +~ ps ]

dealTurnAction :: Kaze -> TurnAction -> Endo Kyoku
dealTurnAction p ta = mconcat $ case ta of 

    TurnTileDiscard dc ->
        [ Endo $ sHands.ix p.handDiscards %~ (|> dc) ]

    TurnTileDraw w mt ->
        [ Endo $ pWallTilesLeft -~ 1
        , Endo $ sWall %~ if' w initEx tailEx ]

    TurnAnkan tile ->
        [ ]
        -- [ Endo $ sHands.ix p.handCalled %~ (|> kantsu tile) ]

    TurnShouminkan t ->
        [ ]
        -- let isShoum m = mentsuKind m == Koutsu && mentsuTile m == t
        -- in [ Endo $ sHands.ix p.handPublic.handCalled.each . filtered isShoum %~ promoteToKantsu ]

    TurnTsumo ->
        [] -- XXX: should something happen?

-- * Waiting

filterCouldShout :: Tile -- ^ Tile to shout
                 -> Kaze -- ^ Whose tile
                 -> Map Kaze HandA
                 -> [(Kaze, Shout)] -- ^ Sorted in correct precedence (highest priority first)
filterCouldShout dt np =
    sortBy (shoutPrecedence np)
    . concatMap flatten . Map.toList . Map.mapWithKey (shoutsOn np dt)
  where flatten (k, xs) = map (k,) xs

waitingShouts :: InKyoku m => Tile -> m [WaitShout]
waitingShouts dt = do
    let secs = 15 -- TODO hard-coded limit

    lastTile <- view pWallTilesLeft <&> (== 0)
    shouts'  <- filterCouldShout dt <$> view pTurn <*> view sHands

    let shouts = map (liftA2 (,) (^?!_head._1) (^..each._2))
                $ groupBy ((==) `on` view _1)
                $ if' lastTile (filter ((`elem` [Ron, Chankan]) . shoutKind . snd)) id shouts'

    players <- mapM (kazeToPlayer . fst) shouts

    return $ zipWith (\p (k, xs) -> (p, k, secs, xs)) players shouts
