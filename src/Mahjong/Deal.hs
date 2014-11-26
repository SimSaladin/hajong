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
import           Data.Monoid (Endo(..))
import qualified Data.List as L (delete)

-- | Context of game and deal flow.
--
-- @Deal@ data type is read-only. Modifications must be encoded in
-- 'GameEvent's. This way it is trivial to keep clients' public states in
-- sync.
type DealM m =
    ( MonadReader Deal m
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
    deal <- ask
    imapM_ (\p (pk,_,_) -> tellEvent . DealStarts p pk $ buildPlayerState deal pk)
          (deal^.pPlayers)
    view pTurn >>= startTurn

-- | n seconds
turnWaiting :: DealM m => Int -> m ()
turnWaiting n = do
    tk <- view pTurn
    tp <- kazeToPlayer tk
    rt <- handOf' tk <&> handCanRiichiWith
    tellEvent $ DealWaitForTurnAction (tp, tk, n, rt)

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
    sh <- handOf' sk
    th <- handOf' tk
    (m, th') <- shoutFromHand sk shout th
    sh' <- meldTo shout m sh
    updateHand sk sh' >> updateHand tk th'
    tellEvent $ DealTurnShouted sk shout
    if shoutKind shout == Ron
        then do
            win <- toWinner sp
            Just <$> dealEnds (DealRon [win] [(tp, win^._2.vhValue.vaValue)])
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
    h <- handOf' pk
    case ta of
        -- TODO no updateHands
        TurnTileDiscard d    -> do
            when (d^.dcTo.to isJust) $ throwError "You cannot specify who shouted your discard when discarding it"
            when (d^.dcRiichi) $ doRiichi pk
            h' <- discard d h
            endTurn (d^.dcTile)
            updateHand pk h'
        TurnAnkan tile       -> ankanOn tile h >>= updateHand pk
        TurnTileDraw False _ -> drawWall h >>= updateHand pk
        TurnTileDraw True  _ -> drawDeadWall h >>= updateHand pk
        TurnTsumo            -> handWin h >>= updateHand pk
        TurnShouminkan tile  -> shouminkanOn tile h

    if ta == TurnTsumo then Just <$> endTsumo
                       else return Nothing

-- *** Player in turn

drawWall :: DealM m => Hand -> m Hand
drawWall hand = do
    unless (canDraw hand) (throwError "Cannot draw from wall")
    mt <- preview (sWall._head)
    maybe (throwError "No tiles left in wall") (`toHand` hand) mt

drawDeadWall :: DealM m => Hand -> m Hand
drawDeadWall hand = do
    mw <- preview (sWall._last)
    case mw of
        Nothing -> throwError "No tiles in wall!"
        Just tow -> do
            t <- view (sWanpai.singular _head)
            tellEvent $ DealFlipDora t (Just tow)
            t `toHandWanpai` hand

doRiichi :: DealM m => Kaze -> m ()
doRiichi pk = do
    p <- kazeToPlayer pk
    np <- view $ pPlayers.at p.singular _Just._2.to (\a -> a - 1000)
    when (np < 0) $ throwError "Cannot riichi: not enough points"
    tell [DealRiichi pk, GamePoints p np]

-- | Set sWaitingShouts to players who could shout the discard.
endTurn :: DealM m => Tile -> m ()
endTurn dt = waitingShouts dt >>= tell . map DealWaitForShout

-- *** Automatic actions

autoDiscard :: DealM m => m ()
autoDiscard = do
    tk <- view pTurn
    void $ runTurn' tk . TurnTileDiscard =<< handAutoDiscard =<< handOf' tk

autoDraw, autoDrawWanpai :: DealM m => m ()
autoDraw = void . flip runTurn' (TurnTileDraw False Nothing) =<< view pTurn
autoDrawWanpai = void . flip runTurn' (TurnTileDraw True Nothing) =<< view pTurn

-- ** Results

endDraw :: DealM m => m DealResults
endDraw = do
    hands <- view sHands
    let (tenpaiPlayers, nootenPlayers) = both.each %~ fst $ partition (tenpai . snd) $ itoList hands
    if null tenpaiPlayers
        then dealEnds $ DealDraw [] []
        else do
            res <- DealDraw <$> mapM kazeToPlayer tenpaiPlayers
                            <*> mapM (kazeToPlayer >=> return . (, 1000)) nootenPlayers
            dealEnds res

endTsumo :: DealM m => m DealResults
endTsumo = do
    tk      <- view pTurn
    players <- view pPlayers <&> map fst . itoList
    tp      <- kazeToPlayer tk
    oja     <- view pOja
    win     <- toWinner tp
    dealEnds $ DealTsumo [win] (tsumoPayers oja (win^._2.vhValue.vaValue) $ L.delete tp players)

dealEnds :: DealM m => DealResults -> m DealResults
dealEnds results = tell [DealEnded results] >> return results

toWinner :: DealM m => Player -> m Winner
toWinner p = do
    pk <- playerToKaze p
    h  <- handOf' pk
    d  <- ask
    return (p, valueHand pk h d)

tsumoPayers :: Player -> Points -> [Player] -> [Payer]
tsumoPayers oja basic payers
    | oja `elem` payers = map (\p -> (p, basic * if' (p == oja) 2 1)) payers
    | otherwise         = map (, basic) payers

----------------------------------------------------------------------------------------

-- * Events

tellPlayerState :: DealM m => Player -> m ()
tellPlayerState p = do
    pk <- playerToKaze p
    deal <- ask
    tellEvent . DealStarts p pk $ buildPlayerState deal pk

updatePlayerNick :: DealM m => Player -> Text -> m ()
updatePlayerNick p nick = playerToKaze p >>= \pk -> tellEvent (DealNick p pk nick)

-- | Build the player's "@GamePlayer@" record, or the state of the game as
-- seen by the player (hide "Deal" but show the player's own hand).
buildPlayerState :: Deal -> Kaze -> AsPlayer
buildPlayerState deal pk = flip appEndo deal $ mconcat
    [ Endo $ sEvents .~ []
    , Endo $ sHands %~ imap (\k -> if' (k == pk) id maskPublicHand)
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
updateHand :: DealM m => Kaze -> Hand -> m ()
updateHand pk new = do
    p <- kazeToPlayer pk
    old <- handOf' pk

    when (old /= new) $ tellEvent (DealPrivateHandChanged p pk new)

    when (_handPublic old /= _handPublic new)
        $ tellEvent (DealPublicHandChanged pk $ _handPublic new)

tellEvent :: DealM m => GameEvent -> m ()
tellEvent ev = tell [ev]

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- DealEvent.
publishTurnAction :: DealM m => Kaze -> TurnAction -> m ()
publishTurnAction pk ra = tellEvent $ case ra of
    TurnTileDraw b _ -> DealTurnAction pk (TurnTileDraw b Nothing)
    _                -> DealTurnAction pk ra

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
handOf' p = view (handOf p) >>= maybe (throwError "handOf': Player not found") return

----------------------------------------------------------------------------------------

-- * Functions

handOf :: Kaze -> Lens Deal Deal (Maybe Hand) (Maybe Hand)
handOf pk = sHands.at pk

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
maybeGameResults Deal{..}
    | minimumOf (traversed._2) _pPlayers < Just 0 = score
    | otherwise = do
        guard (_pRound >= Nan)
        guard (_pPlayers ^? at _pOja._Just._1 == Just Pei)
        guard (maximumOf (traversed._2) _pPlayers > Just 30000)
        score
  where
    score = return . finalPoints $ view _2 <$> _pPlayers

-- ** Waiting

filterCouldShout :: Tile -- ^ Tile to shout
                 -> Kaze -- ^ Whose tile
                 -> Map Kaze Hand
                 -> [(Kaze, Shout)] -- ^ Sorted in correct precedence (highest priority first)
filterCouldShout dt np =
    sortBy (shoutPrecedence np)
    . concatMap flatten . Map.toList . Map.mapWithKey (shoutsOn np dt)
  where flatten (k, xs) = map (k,) xs

waitingShouts :: DealM m => Tile -> m [WaitShout]
waitingShouts dt = do
    let secs = 15 -- ^ TODO hard-coded limit

    lastTile <- view pWallTilesLeft <&> (== 0)
    shouts'  <- filterCouldShout dt <$> view pTurn <*> view sHands

    let shouts = map (liftA2 (,) (^?!_head._1) (^..each._2))
                $ groupBy ((==) `on` view _1)
                $ if' lastTile (filter ((`elem` [Ron, Chankan]) . shoutKind . snd)) id shouts'

    players <- mapM (kazeToPlayer . fst) shouts

    return $ zipWith (\p (k, xs) -> (p, k, secs, xs)) players shouts

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

dealGameEvent :: GameEvent -> Deal -> Deal
dealGameEvent ev = appEndo . mconcat $ case ev of
    DealTurnBegins p
            -> [ Endo $ pTurn .~ p
               , Endo $ sWaiting .~ Nothing ]
    DealTurnAction p ta
            -> [ Endo $ dealTurnAction p ta ]
    DealTurnShouted p _shout
            -> [ Endo $ pTurn .~ p
                {-, Endo $ sHands.ix p %~
                      ( handPublic.handCalled %~ (|> fromShout shout)
                      . handConcealed %~ (L.\\ shoutTo shout)
                      . if' (shoutKind shout == Kan) (handPublic.handDrawWanpai .~ True) id
                      )
               , Endo $ sHands.ix (shoutFrom shout)
                      . handPublic.handDiscards._last.dcTo .~ Just p -}
               ]

    -- TODO get rid these in favor of finer control
    DealPublicHandChanged pk hp -> [ Endo $ sHands.ix pk.handPublic .~ hp ]
    DealPrivateHandChanged p pk h -> [ Endo $ sHands.ix pk .~ h ]

    DealEnded how
            -> [ Endo $ pResults .~ Just how ]
    DealNick p _ nick
            -> [ Endo $ pPlayers.ix p._3 .~ nick ]

    -- player-private
    DealStarts _ _ new
        | null (_sWanpai new) -> [] -- TODO checks if player; the whole function is to work on server for now
        | otherwise           -> [ Endo $ const new ]
    DealWaitForShout ws
            -> [ Endo $ sWaiting %~ Just . Right . maybe [ws] (either (const [ws]) (|> ws)) ]
    DealWaitForTurnAction wt
            -> [ Endo $ sWaiting .~ Just (Left wt) ]
    DealRiichi pk
            -> [ Endo $ do
                    let isShout DealTurnShouted{} = True
                        isShout _ = False
                    db <- liftA2 (&&) (^?!sHands.at pk._Just.handPublic.handDiscards.to null)
                                      (view sEvents <&> not . any isShout)
                    sHands.ix pk.handPublic %~ (handRiichi .~ True) . (hDoubleRiichi .~ db)
               , Endo $ pRiichi +~ 1000
               ]
    DealFlipDora td mtw
            -> [ Endo $ pDora %~ (|> td)
               , Endo $ maybe id (over sWanpai . flip snoc) mtw ]
    GamePoints pk ps
            -> [ Endo $ pPlayers.ix pk._2 .~ ps ]

dealTurnAction :: Kaze -> TurnAction -> Deal -> Deal
dealTurnAction p ta = appEndo . mconcat $ case ta of 
    TurnTileDiscard dc
            -> [ Endo $ sHands.ix p.handPublic.handDiscards %~ (|> dc)
               , if' (dc^.dcRiichi) (Endo $ sHands.ix p.handPublic.handRiichi .~ True) mempty ]
    TurnTileDraw w mt
            -> [ Endo $ pWallTilesLeft -~ 1
               , Endo $ sWall %~ if' w initEx tailEx
               , Endo $ sHands.ix p.handPick .~ mt
               ]
    TurnAnkan tile
            -> [ Endo $ sHands.ix p.handPublic.handCalled %~ (|> kantsu tile) ]
    TurnShouminkan t
            -> [ let isShoum m = mentsuKind m == Koutsu && mentsuTile m == t
                 in Endo $ sHands.ix p.handPublic.handCalled.each
                         . filtered isShoum %~ promoteToKantsu ]
    TurnTsumo
            -> [] -- XXX: should something happen?
