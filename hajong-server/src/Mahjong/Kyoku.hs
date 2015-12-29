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
    ( -- * Types
      InKyoku, Machine(..), MachineInput(..), Flag(..)

    -- * Actions
    , step
    , dealGameEvent
    , updatePlayerNick
    , tellPlayerState
    , maybeNextDeal

    -- * Utility
    , playerToKaze
    , getShouts
    , module Mahjong.Kyoku.Internal
    ) where

------------------------------------------------------------------------------
import           Import
import           Mahjong.Tiles
import           Mahjong.Hand
import           Mahjong.Configuration
------------------------------------------------------------------------------
import           Mahjong.Kyoku.Flags
import           Mahjong.Kyoku.Internal
------------------------------------------------------------------------------
import qualified Data.Map as Map
import           Data.Monoid (Endo(..))
import qualified Data.List as L (delete, findIndex, (!!))

-- | Context of game and deal flow.
--
-- Don't use the state monad yourself! always use tellEvent
type InKyoku m =
    ( MonadState Kyoku m
    , MonadWriter [GameEvent] m
    , MonadError Text m
    , Functor m
    , Applicative m
    , Monad m )

handOf :: Kaze -> Lens Kyoku Kyoku (Maybe HandA) (Maybe HandA)
handOf pk = sHands.at pk

----------------------------------------------------------------------------------------

-- * Logic

-- | Game automata
data Machine = NotBegun Int -- Seconds to wait before continuing. Waiting is not done in this module, so do it somewhere else.
             | CheckEndConditionsAfterDiscard
             | WaitingDraw Kaze Bool
             | WaitingDiscard Kaze
             | WaitingShouts (Set Kaze) (Maybe [Int]) [(Kaze, Shout)] Bool -- ^ set of players who could shout but have not (passed), index of now winning shout(s), flag: chankan? (to continue with discard)
             | KyokuEnded KyokuResults
             deriving (Eq, Show, Read)

data MachineInput = InpAuto
                  | InpTurnAction Kaze TurnAction
                  | InpShout Kaze Shout
                  | InpPass Kaze
                  deriving (Show, Read)

-- | Remember to publish the turn action when successful
step :: InKyoku m => Machine -> MachineInput -> m Machine
step (NotBegun _) InpAuto                   = sendDealStarts >> waitForDraw -- startDeal

step (WaitingDraw pk wanpai) InpAuto        = draw pk wanpai >> askForTurnAction 15 >> return (WaitingDiscard pk) -- TODO hard-coded timeout
step (WaitingDraw pk wanpai) (InpTurnAction pk' (TurnTileDraw wanpai' _))
            | wanpai /= wanpai'             = throwError "You are not supposed to draw there"
            | pk /= pk'                     = throwError $ "Not your (" ++ tshow pk' ++ ") turn, it's turn of " ++ tshow pk
            | otherwise                     = draw pk wanpai >> askForTurnAction 15 >> return (WaitingDiscard pk) -- TODO hard-coded timeout

step (WaitingDiscard pk) InpAuto            = autoDiscard pk
step (WaitingDiscard pk) (InpTurnAction pk' ta)
            | pk /= pk'                     = throwError $ "Not your (" ++ tshow pk' ++ ") turn, it's turn of " ++ tshow pk
            | TurnTileDiscard d <- ta       = processDiscard pk d
            | TurnAnkan t <- ta             = processAnkan pk t
            | TurnTsumo <- ta               = endTsumo pk <&> KyokuEnded
            | TurnShouminkan t <- ta        = processShouminkan pk t

step (WaitingShouts couldShout winning shouts chankan) inp
            | InpAuto <- inp, Just xs <- winning = processShouts (map (shouts L.!!) xs) chankan
            | InpAuto <- inp                     = proceedWithoutShoutsAfterDiscard chankan
            | null couldShout                    = proceedWithoutShoutsAfterDiscard chankan
            | InpPass pk <- inp                  = return $ WaitingShouts (deleteSet pk couldShout) winning shouts chankan
            | InpShout pk shout <- inp, Just i <- L.findIndex (== (pk, shout)) shouts
            = do
                res <- use pTurn >>= \tk -> case winning of
                    Just (j:js) -> case shoutPrecedence tk (shouts L.!! j) (pk, shout) of
                                       EQ -> return $ WaitingShouts (deleteSet pk couldShout) (Just (i:j:js)) shouts chankan -- new goes through with old ones
                                       GT -> return $ WaitingShouts couldShout                (Just (j:js))   shouts chankan -- old takes precedence (XXX: this branch should never even be reached
                                       LT -> return $ WaitingShouts (deleteSet pk couldShout) (Just [i]) shouts chankan -- new takes precedence
                    Nothing     ->           return $ WaitingShouts (deleteSet pk couldShout) (Just [i]) shouts chankan
                case res of
                    WaitingShouts couldShout _ _ _ | null couldShout -> step res InpAuto
                    _ -> return res
            | otherwise = throwError "No such call is possible"

step CheckEndConditionsAfterDiscard InpAuto = checkEndConditions

step (KyokuEnded{}) _ = throwError "This kyoku has ended!"
step st inp           = throwError $ "Kyoku.step: Invalid input in state " <> tshow st <> ": " <> tshow inp

-- | chankan?
proceedWithoutShoutsAfterDiscard :: InKyoku m => Bool -> m Machine
proceedWithoutShoutsAfterDiscard chankan = if' chankan (WaitingDiscard <$> use pTurn) (return CheckEndConditionsAfterDiscard)

dealGameEvent :: GameEvent -> Kyoku -> Kyoku
dealGameEvent ev = appEndo . mconcat $ case ev of

    DealTurnBegins p ->
        [ Endo $ pTurn .~ p
        , Endo $ sWaiting .~ Nothing ]

    DealTurnAction p ta ->
        [ dealTurnAction p ta ]

    DealTurnShouted p _shout ->
        [ Endo $ pTurn .~ p ]

    -- TODO get rid these in favor of finer control?
    DealPublicHandChanged _pk _hp -> [ ]
    --     [ Endo $ sHands.ix pk.handPublic .~ hp ]

    DealPrivateHandChanged _ pk h ->
        [ Endo $ sHands.ix pk .~ h ]

    DealEnded how ->
        [ Endo $ pResults .~ Just how ]

    DealNick pk player nick ->
        [ Endo $ pPlayers . ix pk . _3 .~ nick
        , Endo $ pPlayers . ix pk . _1 .~ player ]

    -- player-private
    DealStarts _ _ _ -> [ ]

    DealWaitForShout ws ->
        [ Endo $ sWaiting %~ Just . Right . maybe [ws] (either (const [ws]) (|> ws)) ]

    DealWaitForTurnAction wt ->
        [ Endo $ sWaiting .~ Just (Left wt) ]

    DealRiichi _pk ->
        [ Endo $ pRiichi +~ 1000 ]

    DealFlipDora td -> [ Endo $ pDora %~ (|> td) ]

    GamePoints pk ps ->
        [ Endo $ pPlayers.ix pk._2 +~ ps ]

dealTurnAction :: Kaze -> TurnAction -> Endo Kyoku
dealTurnAction p ta = mconcat $ case ta of 

    TurnTileDiscard dc ->
        [ Endo $ sHands.ix p.handDiscards %~ (|> dc) ]

    TurnTileDraw w _ ->
        [ Endo $ pWallTilesLeft -~ 1 ]

    TurnAnkan _ ->
        [ ]
        -- [ Endo $ sHands.ix p.handCalled %~ (|> kantsu tile) ]

    TurnShouminkan _ ->
        [ ]
        -- let isShoum m = mentsuKind m == Koutsu && mentsuTile m == t
        -- in [ Endo $ sHands.ix p.handPublic.handCalled.each . filtered isShoum %~ promoteToKantsu ]

    TurnTsumo ->
        [] -- XXX: should something happen?

-- | Results are returned if west or higher round has just ended and
-- someone is winning (over 30000 points).
maybeGameResults :: Kyoku -> Maybe FinalPoints
maybeGameResults kyoku@Kyoku{..}
    | minimumOf (traversed._2) _pPlayers < Just 0 = return score
    | otherwise = do
        guard (fst _pRound >= Nan)
        guard (nextRound kyoku ^. _1 > Nan)
        guard (maximumOf (traversed._2) _pPlayers > Just 30000)
        return score
  where
    score = finalPoints $ mapFromList $ _pPlayers ^.. each.to (\(p,ps,_) -> (p, ps))

nextRound :: Kyoku -> Round
nextRound Kyoku{..}
    | Just DealAbort{..} <- _pResults = _pRound & _2 +~ 1
    | Just DealDraw{..}  <- _pResults = if Ton `elem` (dNooten^..each._1)  then _pRound & _2 +~ 1 else (_pRound & fst & nextKaze, 1)
    | Just DealRon{..}   <- _pResults = if Ton `elem` (dWinners^..each._1) then _pRound & _2 +~ 1 else (_pRound & fst & nextKaze, 1)
    | Just DealTsumo{..} <- _pResults = if Ton `elem` (dWinners^..each._1) then _pRound & _2 +~ 1 else (_pRound & fst & nextKaze, 1)
    | Nothing            <- _pResults = _pRound

-- | Advance the game to next deal, or end it.
-- TODO move to Round.hs
maybeNextDeal :: Kyoku -> IO (Either FinalPoints Kyoku)
maybeNextDeal deal = maybe (Right <$> nextDeal deal) (return . Left) $ maybeGameResults deal

-- | TODO move to Round.hs. Everything but tile shuffling could be in
-- events.
nextDeal :: Kyoku -> IO Kyoku
nextDeal kyoku = do tiles <- shuffleTiles
                    return $ go $ dealTiles tiles kyoku
  where
    go = set pRound newRound
       . set pTurn Ton
       . if' rotate (pOja .~ nextOja) id
       . if' rotate (over pPlayers rotatePlayers) id
       . if' honbaResets (pHonba .~ 0) (pHonba +~ 1)
       . if' riichiTransfers (pRiichi .~ 0) id
       . set pResults Nothing
       . over pDeals (cons $ _pRound kyoku)

    newRound    = nextRound kyoku
    rotate      = newRound^._2 == 1
    honbaResets = case kyoku^?!pResults of
                      Just DealTsumo{..} -> notElemOf (each._1) Ton dWinners
                      Just DealRon{..}   -> notElemOf (each._1) Ton dWinners
                      _                  -> False
    riichiTransfers = case kyoku^?!pResults of
                          Just DealAbort{} -> False
                          Just DealDraw{} -> False
                          _ -> True
    nextOja       = kyoku ^?! pPlayers.ix Nan . _1 :: Player

rotatePlayers :: Map Kaze a -> Map Kaze a
rotatePlayers = mapFromList . map (_1 %~ prevKaze) . mapToList

nagashiOrDraw :: InKyoku m => m Machine
nagashiOrDraw = do
    hands <- use sHands
    wins  <- iforM hands $ \pk hand -> if handInNagashi hand then return $ handWinsNagashi pk hand else return Nothing

    let winners = map fst $ catMaybes $ map snd $ mapToList wins :: [Winner]
        payers  = map (\xs@((p,_):_) -> (p, sumOf (traversed._2) xs)) $ groupBy ((==) `on` fst) $ (concatMap snd $ catMaybes $ map snd $ mapToList wins) :: [Payer]

    if null winners then KyokuEnded <$> endDraw else return $ KyokuEnded $ DealTsumo winners payers

handWinsNagashi :: Kaze -> Hand Identity -> Maybe (Winner, [Payer])
handWinsNagashi pk hand =
    -- TODO perhaps this could be placed in YakuCheck too
     let points = floor $ if' (pk == Ton) 1.5 1 * 8000
         value  = Value [Yaku 5 "Nagashi Mangan"] 0 5 0 (Just "Mangan")
         winner = (pk, points, ValuedHand (hand^.handCalled) (hand^.handConcealed._Wrapped) value)

         -- TODO fails when not four players
         payers = map (\payer -> (payer, if' (payer == Ton) 2 1 * 2000)) $ L.delete pk [Ton .. Pei]
         in Just (winner, payers)

processAnkan :: InKyoku m => Kaze -> Tile -> m Machine
processAnkan pk t = do
    handOf' pk >>= ankanOn t >>= updateHand pk
    otherHands <- use sHands <&> filter ((/=pk).fst) . Map.toList
    let kokushiWins = filter ((== NotFuriten) . runIdentity . _handFuriten . snd) $
                      filter ((== Just (-1)) . shantenBy kokushiShanten . (handConcealed._Wrapped %~ cons t) . snd) otherHands
    if null kokushiWins -- chankan on ankan only when kokushi could win from it
        then return $ WaitingDraw pk True
        else return $ WaitingShouts (setFromList $ map fst kokushiWins) Nothing (each._2 .~ Shout Chankan pk t [] $ kokushiWins) True

processShouminkan :: InKyoku m => Kaze -> Tile -> m Machine
processShouminkan pk t = do
    handOf' pk >>= shouminkanOn t >>= updateHand pk
    chankanShouts <- getShouts t <&> over (each._2) toChankan . filter ((== Ron) . shoutKind . snd)
    if null chankanShouts then return (WaitingDraw pk True)
                          else do tellEvents . map DealWaitForShout =<< toWaitShouts chankanShouts
                                  return (WaitingShouts (setFromList $ map fst chankanShouts) Nothing chankanShouts True)
 where
   toChankan s = s { shoutKind = Chankan }

checkEndConditions :: InKyoku m => m Machine
checkEndConditions = do
    updateTempFuritens

    tilesLeft      <- use pWallTilesLeft
    dora           <- use pDora
    everyoneRiichi <- use sHands <&> allOf (each.handRiichi) (/= NoRiichi)
    firstDiscards  <- use sHands <&> toListOf (each.handDiscards._head.dcTile)

    let fourWindTiles = length firstDiscards == 4 && isKaze (headEx firstDiscards) && allSame firstDiscards

    case () of
        _ | length dora == 5 -> return $ KyokuEnded $ DealAbort SuuKaikan -- TODO check that someone is not waiting for the yakuman
          | tilesLeft == 0   -> nagashiOrDraw
          | everyoneRiichi   -> return $ KyokuEnded $ DealAbort SuuchaRiichi
          | fourWindTiles    -> return $ KyokuEnded $ DealAbort SuufonRenda
          | otherwise        -> advanceTurn <&> (`WaitingDraw` False)
  where
     allSame :: [Tile] -> Bool
     allSame [] = True
     allSame (x:xs) = all (x ==~) xs

-- ** Beginning

-- | Send the very first Kyoku events to everyone. Contains the game state.
sendDealStarts :: InKyoku m => m ()
sendDealStarts = do
    deal <- get
    imapM_ (\pk (player,_,_) -> tellEvent . DealStarts player pk $ buildPlayerState deal pk) (deal^.pPlayers)

-- ** Drawing

-- | WaitingDraw, not wanpai
waitForDraw :: InKyoku m => m Machine
waitForDraw = do
    pk <- use pTurn
    h <- handOf' pk
    updateHand pk $ h & handState .~ DrawFromWall
    tellEvent $ DealTurnBegins pk
    return $ WaitingDraw pk False

draw :: InKyoku m => Kaze -> Bool -> m ()
draw pk wanpai = do

    firstRound <- use $ pFlags.to (member FirstRoundUninterrupted)
    when firstRound $ do
        allHandsInterrupted <- use sHands <&> allOf (each.handFlags._Wrapped) (notMember HandFirsRoundUninterrupted)
        when allHandsInterrupted $ unsetFlag FirstRoundUninterrupted

    updateHand pk =<< (if wanpai then drawDeadWall else drawWall) =<< handOf' pk
    tellEvent $ DealTurnAction pk $ TurnTileDraw wanpai Nothing

drawWall :: InKyoku m => HandA -> m HandA
drawWall hand = do
    wallHead <- preuse (sWall._head) >>= maybe (throwError "Wall is empty") return
    sWall %= tailEx
    wallHead `toHand` hand

drawDeadWall :: InKyoku m => HandA -> m HandA
drawDeadWall hand = do
    lastTileInWall <- preuse (sWall._last) >>= maybe (throwError "Wall is empty") return
    fromWanpai     <- wanpaiGetSupplement lastTileInWall
    flipNewDora -- TODO should it flip now or after the discard?
    fromWanpai `toHandWanpai` hand

-- ** Discarding

processDiscard :: InKyoku m => Kaze -> Discard -> m Machine
processDiscard pk d' = do
    let d = d' & dcTo .~ Nothing
    when (d^.dcRiichi) $ doRiichi pk
    updateHand pk =<< discard d =<< handOf' pk

    shouts <- getShouts (d^.dcTile)
    waiting <- toWaitShouts shouts
    tellEvents $ map DealWaitForShout waiting
    return $ if null waiting
                 then CheckEndConditionsAfterDiscard
                 else WaitingShouts (setFromList $ map fst shouts) Nothing shouts False

doRiichi :: InKyoku m => Kaze -> m ()
doRiichi pk = do
    pointsAfterRiichi <- use $ pPlayers.at pk.singular _Just._2.to (\a -> a - 1000)
    when (pointsAfterRiichi < 0) $ throwError "Cannot riichi: not enough points"
    tellEvents [DealRiichi pk, GamePoints pk (-1000)]

autoDiscard :: InKyoku m => Kaze -> m Machine
autoDiscard tk = processDiscard tk =<< handAutoDiscard =<< handOf' tk

-- | Update temporary furiten state of all players after a discard.
updateTempFuritens :: InKyoku m => m ()
updateTempFuritens = do
    pk   <- use pTurn
    tile <- _dcTile . lastEx . _handDiscards <$> handOf' pk
    handOf' pk >>= \h -> case h^.handFuriten._Wrapped of
        NotFuriten | tile `elem` handGetAgari h -> updateHand pk $ h & handFuriten._Wrapped .~ TempFuriten
        TempFuriten                             -> updateHand pk $ h & handFuriten._Wrapped .~ NotFuriten
        _                                       -> return ()
    forM_ (L.delete pk [Ton .. Pei]) $ \k -> handOf' k >>= \h -> case h^.handFuriten._Wrapped of
        NotFuriten | tile `elem` handGetAgari h -> updateHand k $ h & handFuriten._Wrapped .~ TempFuriten
        _                                       -> return ()

-- ** Turn-passing

-- | Next player who draws a tile
advanceTurn :: InKyoku m => m Kaze
advanceTurn = do
    pk <- use pTurn <&> nextKaze
    updateHand pk . set handState DrawFromWall =<< handOf' pk
    tellEvent $ DealTurnBegins pk
    return pk

-- ** Wanpai

-- | Flip a new dora from wanpai. If the kyoku would end in suukaikan after
-- the next discard, set the @SuuKaikanAfterDiscard@ flag.
--
-- Note: does not check if the filp is valid.
flipNewDora :: InKyoku m => m ()
flipNewDora = tellEvent . DealFlipDora =<< wanpaiGetDora

-- | Reveals opened ura-dora from the wanpai. Toggles the @OpenedUraDora@
-- flag.
revealUraDora :: InKyoku m => m ()
revealUraDora = do
    count <- use (pDora.to length)
    use (sWanpai.wUraDora) >>= setFlag . OpenedUraDora . map TileEq . take count

-- | Get supplementary tile from wanpai. Argument must be a tile from
-- the wall to append to the wall.
wanpaiGetSupplement :: InKyoku m => Tile -> m Tile
wanpaiGetSupplement fromWall = preuse (sWanpai.wSupplement._Cons) >>= \case
    Just (x, xs) -> do sWanpai.wSupplement .= xs
                       sWanpai.wBlank %= flip snoc fromWall
                       return x
    Nothing      -> throwError "Four kantsu already called"

wanpaiGetDora :: InKyoku m => m Tile
wanpaiGetDora = preuse (sWanpai.wDora._Cons) >>= \case
    Just (x, xs) -> sWanpai.wDora .= xs >> return x
    Nothing      -> throwError "Internal error: no dora left in wanpai"

-- ** Waiting

-- | n seconds
askForTurnAction :: InKyoku m => Int -> m ()
askForTurnAction n = do
    tk <- use pTurn
    tp <- kazeToPlayer tk
    rt <- handOf' tk <&> handCanRiichiWith
    tellEvent $ DealWaitForTurnAction (tp, tk, n, rt)

-- | @processShout shouts chankan?@
--
-- Multiple shouts are allowed only when they go out
processShouts :: InKyoku m => [(Kaze, Shout)] -> Bool -> m Machine
processShouts shouts@((fsk,fs):_) chank = do
    tk <- use pTurn
    th <- handOf' tk

    (th':_) <- forM shouts $ \(sk, s) -> do
        sh <- handOf' sk
        (m, th') <- shoutFromHand sk s th -- TODO: split shoutFromHand to two functions so it only needs to be run once
        sh' <- if null (shoutTo s) then handWin (Just s) sh else meldTo s m sh -- if null then kokushi. can't use a mentsu 'cause mentsu have >=2 tiles
        updateHand sk sh'
        tellEvent $ DealTurnShouted sk s
        return th'

    updateHand tk th'

    case () of
        _ | shoutKind fs `elem` [Ron, Chankan] -> endRon (map fst shouts) tk <&> KyokuEnded
          | shoutKind fs == Kan -> unsetFlag FirstRoundUninterrupted >> return (WaitingDraw fsk True)
          | otherwise -> do
              unsetFlag FirstRoundUninterrupted
              tellEvent $ DealTurnBegins fsk
              return (WaitingDiscard fsk)

-- ** Ending

endTsumo :: InKyoku m => Kaze -> m KyokuResults
endTsumo winner = do
    handOf' winner >>= handWin Nothing >>= updateHand winner -- FIXME this shouldn't exist, I think

    vh     <- getValuedHand winner
    honba  <- use pHonba
    riichi <- use pRiichi
    payers <- use pPlayers <&> tsumoPayers honba (vh^.vhValue.vaValue) . L.delete winner . map fst . itoList

    let win = (winner, - sumOf (each._2) payers + riichi, vh)

    dealEnds $ DealTsumo [win] payers

endDraw :: InKyoku m => m KyokuResults
endDraw = do
    hands <- use sHands
    let x@(tp, np) = both.each %~ fst $ partition (tenpai . snd) $ itoList hands
    let (r, p) | null tp || null np = (0, 0)
               | otherwise          = x & both %~ div 3000 . fromIntegral . length
    dealEnds $ DealDraw (map (,r) tp) (map (,-p) np)

endRon :: InKyoku m => [Kaze] -> Kaze -> m KyokuResults
endRon winners payer = do
    valuedHands <- mapM getValuedHand winners
    honba       <- use pHonba
    riichi      <- use pRiichi

    let pointsForHonba  = honba * 300
        pointsFromPayer = map (+ pointsForHonba) $ zipWith valuedHandPointsForRon winners valuedHands
        winEntries      = zip3 winners pointsFromPayer valuedHands

    dealEnds $ DealRon winEntries [(payer, negate $ sum pointsFromPayer)]

-- *** Helpers

valuedHandPointsForRon :: Kaze -> ValuedHand -> Points
valuedHandPointsForRon pk vh = roundKyokuPoints $ if' (pk == Ton) 6 4 * (vh^.vhValue.vaValue)

-- | Apply KyokuResults and pay points.
dealEnds :: InKyoku m => KyokuResults -> m KyokuResults
dealEnds results = do
    tellEvents $ DealEnded results : payPoints results
    return results

-- | Value the hand. If the hand would be valueless (no yaku, just extras),
-- raise an error.
getValuedHand :: InKyoku m => Kaze -> m ValuedHand
getValuedHand pk = do
    revealUraDora
    vh <- valueHand pk <$> handOf' pk <*> get
    when (length (vh^..vhValue.vaYaku.each.filtered yakuNotExtra) == 0) $
        throwError $ "Need at least one yaku that ain't dora to win.\nThe ValuedHand: " ++ tshow vh
    return vh
  where
    yakuNotExtra Yaku{}      = True
    yakuNotExtra YakuExtra{} = False

-- ** Scoring

payPoints :: KyokuResults -> [GameEvent]
payPoints res = case res of
    DealAbort{}  -> []
    DealDraw{..} -> map (uncurry GamePoints) $ dTenpais ++ dNooten
    _            -> map f (dWinners res) ++ map (uncurry GamePoints) (dPayers res)
  where f (p, v, _) = GamePoints p v

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
-- @tsumoPayers honba winner basicPoints payers@
--
-- >>> tsumoPayers 0 320 [Nan, Shaa, Pei]
-- [(Nan,-700),(Shaa,-700),(Pei,-700)]
--
-- >>> tsumoPayers 1 320 [Ton, Shaa, Pei]
-- [(Ton,-800),(Shaa,-500),(Pei,-500)]
--
tsumoPayers :: Int -> Points -> [Kaze] -> [Payer]
tsumoPayers honba basic payers
    | Ton `elem` payers = map (\p -> (p, negate $ roundKyokuPoints $ basic * if' (p == Ton) 2 1 + honba * 100)) payers
    | otherwise         = map (        , negate $ roundKyokuPoints $ basic * 2                  + honba * 100) payers

-- * Final scoring

-- |
-- >>> finalPoints (Map.fromList [(Player 0,25000), (Player 1,25000), (Player 2,25000), (Player 3,25000)])
-- FinalPoints (fromList [(Player 0,35),(Player 1,5),(Player 2,-15),(Player 3,-25)])
--
-- >>> finalPoints (Map.fromList [(Player 0, 35700), (Player 1, 32400), (Player 2, 22200), (Player 3, 9700)])
-- FinalPoints (fromList [(Player 0,46),(Player 1,12),(Player 2,-18),(Player 3,-40)])
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
    deal <- get
    tellEvent . DealStarts p pk $ buildPlayerState deal pk

updatePlayerNick :: InKyoku m => Player -> Text -> m ()
updatePlayerNick p nick = playerToKaze p >>= \pk -> tellEvent (DealNick pk p nick)

-- | Build the player's "@GamePlayer@" record, or the state of the game as
-- seen by the player (hide "Deal" but show the player's own hand).
buildPlayerState :: Kyoku -> Kaze -> AsPlayer
buildPlayerState deal pk = flip appEndo (deal { _sHands = imap (\k -> if pk == k then convertHand else maskPublicHand) (_sHands deal) }) $ mconcat
    [ Endo $ sEvents .~ []
    , Endo $ sWall .~ []
    , Endo $ sWanpai .~ Wanpai [] [] [] []
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
updateHand pk new = do
    p   <- kazeToPlayer pk
    old <- handOf' pk
    sHands.at pk .= Just new
    {- when (old /= new) $ -}
    tellEvent (DealPrivateHandChanged p pk new)
    {- when (maskPublicHand old /= maskPublicHand new) $ -}
    tellEvent (DealPublicHandChanged pk $ maskPublicHand new)

tellEvent :: InKyoku m => GameEvent -> m ()
tellEvent ev = do
    modify $ dealGameEvent ev
    tell [ev]

tellEvents :: InKyoku m => [GameEvent] -> m ()
tellEvents = mapM_ tellEvent

----------------------------------------------------------------------------------------

-- * Query info

playerToKaze :: InKyoku m => Player -> m Kaze
playerToKaze player = do
    mp <- use pPlayers <&> ifind (\_ x -> x^._1 == player)
    maybe (throwError $ "Player `" ++ tshow player ++ "' not found") (return . fst) mp

kazeToPlayer :: InKyoku m => Kaze -> m Player
kazeToPlayer pk = do
    rp <- use pPlayers <&> view (at pk)
    maybe (throwError $ "Player `" ++ tshow pk ++ "' not found") (return . (^._1)) rp

handOf' :: InKyoku m => Kaze -> m HandA
handOf' p = use (handOf p) >>= maybe (throwError "handOf': Player not found") return

----------------------------------------------------------------------------------------

-- * Waiting

-- | Get all possible shouts from all players for a given tile.
filterCouldShout :: Tile            -- ^ Tile to shout
                 -> Kaze            -- ^ Whose tile
                 -> Map Kaze HandA  -- ^ All hands
                 -> [(Kaze, Shout)] -- ^ Sorted in correct precedence (highest priority as head)
filterCouldShout dt np = sortBy (shoutPrecedence np)
    . concatMap flatten . Map.toList . Map.mapWithKey (shoutsOn np dt) . Map.filter ((== NotFuriten) . runIdentity . _handFuriten)
  where flatten (k, xs) = map (k,) xs

getShouts :: InKyoku m => Tile -> m [(Kaze, Shout)]
getShouts dt = do
    lastTile <- use pWallTilesLeft <&> (== 0)
    shouts   <- filterCouldShout dt <$> use pTurn <*> use sHands
    return $ if' lastTile (filter ((`elem` [Ron, Chankan]) . shoutKind . snd)) id shouts
        -- when there are no tiles, only go-out shouts are allowed

toWaitShouts :: InKyoku m => [(Kaze, Shout)] -> m [WaitShout]
toWaitShouts shouts = do
    let grouped = map (liftA2 (,) (^?!_head._1) (^..each._2)) $ groupBy ((==) `on` view _1) shouts
    forM grouped $ \(k, s) -> do
        player <- kazeToPlayer k
        return (player, k, 15, s)

-- * Flags

unsetFlag, setFlag :: InKyoku m => Flag -> m ()
setFlag f   = void $ pFlags %= insertSet f
unsetFlag f = void $ pFlags %= deleteSet f
