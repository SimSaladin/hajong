{-# LANGUAGE TupleSections, RecordWildCards, DeriveGeneric #-}
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
    , nextRound
    , getValuedHand
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

handOf :: Kaze -> Lens Kyoku Kyoku (Maybe Hand) (Maybe Hand)
handOf pk = sHands.at pk

----------------------------------------------------------------------------------------

-- * Logic

-- | Game automata
data Machine = KyokuNone -- ^ No tiles in the table atm
             | KyokuStartIn Int -- ^ Number of seconds before kyoku starts. Tiles dealt.
             | CheckEndConditionsAfterDiscard
             | WaitingDraw Kaze Bool
             | WaitingDiscard Kaze
             | WaitingShouts (Set Kaze) [Int] [(Kaze, Shout)] Bool -- ^ set of players who could shout but have not (passed), index of now winning shout(s), flag: chankan? (to continue with discard)
             | KyokuEnded KyokuResults
             | HasEnded FinalPoints -- ^ This game has ended
             deriving (Eq, Show, Read, Generic)

-- | @MachineInput@ consists of two parts: actions from clients a la
-- @GameAction@, and actions from the managing process i.e. advances to
-- next rounds and automatic advances after timeouts.
data MachineInput = InpAuto -- ^ Whatever the current state is, do an action that advances it to the next state
                  | InpTurnAction Kaze TurnAction -- ^ An action from player in turn
                  | InpShout Kaze Shout -- ^ A call
                  | InpPass Kaze -- ^ Ignore call
                  deriving (Show, Read)

-- | Remember to publish the turn action when successful
step :: InKyoku m => Machine -> MachineInput -> m Machine
step KyokuNone _ = return KyokuNone
step (KyokuStartIn _) InpAuto = sendDealStarts >> waitForDraw -- startDeal

step (WaitingDraw pk wanpai) InpAuto = draw pk wanpai >> askForTurnAction 15 >> return (WaitingDiscard pk) -- TODO hard-coded timeout
step (WaitingDraw pk wanpai) (InpTurnAction pk' (TurnTileDraw wanpai' _))
    | wanpai /= wanpai' = throwError "You are not supposed to draw there"
    | pk /= pk'         = throwError $ "Not your (" ++ tshow pk' ++ ") turn, it's turn of " ++ tshow pk
    | otherwise         = draw pk wanpai >> askForTurnAction 15 >> return (WaitingDiscard pk) -- TODO hard-coded timeout

step (WaitingDiscard pk) InpAuto = autoDiscard pk
step (WaitingDiscard pk) (InpTurnAction pk' ta)
    | pk /= pk'               = throwError $ "Not your (" ++ tshow pk' ++ ") turn, it's turn of " ++ tshow pk
    | TurnTileDiscard d <- ta = processDiscard pk d
    | TurnAnkan t <- ta       = processAnkan pk t
    | TurnTsumo <- ta         = endKyoku =<< endTsumo pk
    | TurnShouminkan t <- ta  = processShouminkan pk t

step (WaitingShouts _ winning shouts chankan) InpAuto
    | null winning = proceedWithoutShoutsAfterDiscard chankan
    | otherwise    = processShouts (map (shouts L.!!) winning) chankan

step (WaitingShouts couldShout winning shouts chankan) (InpPass pk) = do
    let couldShout' = deleteSet pk couldShout
    p <- kazeToPlayer pk
    tellEvent $ DealWaitForShout (p, pk, 0, [])
    if' (null couldShout') (flip step InpAuto) return $ WaitingShouts couldShout' winning shouts chankan

step (WaitingShouts couldShout winning shouts chankan) (InpShout pk shout)
    | pk `onotElem` couldShout, elemOf (each._1) pk shouts = throwError "You have already called on that tile"
    | Just i <- L.findIndex (== (pk, shout)) shouts        = do
        res <- use pTurn >>= \tk -> case winning of
            j:js -> case shoutPrecedence tk (shouts L.!! j) (pk, shout) of -- XXX: Would be prettier with a view-pattern
                EQ      -> return $ WaitingShouts (deleteSet pk couldShout) (i:j:js) shouts chankan -- new goes through with old ones
                GT      -> return $ WaitingShouts couldShout                (j:js)   shouts chankan -- old takes precedence (XXX: this branch should never even be reached
                LT      -> return $ WaitingShouts (deleteSet pk couldShout) [i]      shouts chankan -- new takes precedence
            []          -> return $ WaitingShouts (deleteSet pk couldShout) [i]      shouts chankan

        p <- kazeToPlayer pk
        tellEvent $ DealWaitForShout (p, pk, 0, [])
        case res of
            WaitingShouts couldShout' _ _ _ | null couldShout' -> step res InpAuto
            _ -> return res
    | otherwise = throwError $ "Call '" ++ tshow shout ++ "' not possible (Your possible calls at the moment are: "
            ++ tshow (filter ((==pk).fst) shouts) ++ ")"

step CheckEndConditionsAfterDiscard InpAuto = checkEndConditions

step (KyokuEnded{}) InpAuto = do
    k <- get
    case maybeGameResults k of
        Nothing -> return KyokuNone
        Just res -> endGame res

step HasEnded{} _     = throwError "This game has ended!"
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
    DealStarts{} -> [ ]

    DealWaitForShout ws ->
        [ Endo $ sWaiting %~ Just . Right . maybe [ws] (either (const [ws]) (|> ws)) ]

    DealWaitForTurnAction wt ->
        [ Endo $ sWaiting .~ Just (Left wt) ]

    DealRiichi _pk ->
        [ Endo $ pRiichi +~ 1000 ]

    DealFlipDora td -> [ Endo $ pDora %~ (|> td) ]

    GamePoints pk ps ->
        [ Endo $ pPlayers.ix pk._2 +~ ps ]

    GameEnded _ -> []

dealTurnAction :: Kaze -> TurnAction -> Endo Kyoku
dealTurnAction p ta = mconcat $ case ta of

    TurnTileDiscard dc ->
        [ Endo $ sHands.ix p.handDiscards %~ (|> dc) ]

    TurnTileDraw _w _ ->
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
    | minimumOf (traversed._2) _pPlayers < Just 0 = return score -- if anyone below zero end immediately
    | otherwise = do
        guard (nextRound kyoku ^. _1 > Nan) -- Don't end before *last* south deal
        unless (nextRound kyoku ^. _1 > Shaa) $ -- Require point-difference, unless last deal of an extra-round ended
            guard (maximumOf (traversed._2) _pPlayers > Just 30000)
        return score
  where
    score = finalPoints $ mapFromList $ _pPlayers ^.. each.to (\(p,ps,_) -> (p, ps))

nextRound :: Kyoku -> Round
nextRound Kyoku{..} = case _pResults of
    Just DealAbort{..} -> _pRound & _3 +~ 1
    Just DealDraw{..}  -> _pRound & if elemOf (each._1) Ton dTenpais then _3 +~ 1 else set _3 (_pRound^._3 + 1) . roundRotates
    Just DealRon{..}   -> _pRound & if elemOf (each._1) Ton dWinners then _3 +~ 1 else roundRotates
    Just DealTsumo{..} -> _pRound & if elemOf (each._1) Ton dWinners then _3 +~ 1 else roundRotates
    Nothing            -> _pRound
  where
    roundRotates (k, 4, _) = (succCirc k, 1, 0)
    roundRotates (k, r, _) = (k, r + 1, 0)

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
       . set pHonba (newRound^._3)
       . if' rotate (pOja .~ nextOja) id
       . if' rotate (over pPlayers rotatePlayers) id
       . if' riichiTransfers (pRiichi .~ 0) id
       . set pResults Nothing

    newRound    = nextRound kyoku
    rotate      = newRound^._2 /= kyoku^.pRound._2
    riichiTransfers = case kyoku^?!pResults of
                          Just DealAbort{} -> False
                          Just DealDraw{} -> False
                          _ -> True
    nextOja       = kyoku ^?! pPlayers.ix Nan . _1 :: Player

rotatePlayers :: Map Kaze a -> Map Kaze a
rotatePlayers = mapFromList . map (_1 %~ predCirc) . mapToList

nagashiOrDraw :: InKyoku m => m Machine
nagashiOrDraw = do
    hands <- use sHands
    wins  <- iforM hands $ \pk hand -> if handInNagashi hand then return $ handWinsNagashi pk hand else return Nothing

    let winners = map fst $ catMaybes $ map snd $ mapToList wins :: [Winner]
        payers  = map (\xs@((p,_):_) -> (p, sumOf (traversed._2) xs)) $ groupBy (equating fst) $ (concatMap snd $ catMaybes $ map snd $ mapToList wins) :: [Payer]

    res <- if null winners then endDraw else return $ DealTsumo winners payers
    endKyoku res


handWinsNagashi :: Kaze -> Hand -> Maybe (Winner, [Payer])
handWinsNagashi pk hand =
    -- TODO perhaps this could be placed in YakuCheck too
     let points = floor $ if' (pk == Ton) 1.5 1 * 8000
         value  = Value [Yaku 5 "Nagashi Mangan"] 0 5 0 (Just "Mangan")
         winner = (pk, points, ValuedHand (hand^.handCalled) (hand^.handConcealed) value)

         -- TODO fails when not four players
         payers = map (\payer -> (payer, if' (payer == Ton) 2 1 * 2000)) $ L.delete pk [Ton .. Pei]
         in Just (winner, payers)

processAnkan :: InKyoku m => Kaze -> Tile -> m Machine
processAnkan pk t = do
    handOf' pk >>= ankanOn t >>= updateHand pk
    otherHands <- use sHands <&> filter ((/=pk).fst) . Map.toList
    let kokushiWins = filter ((== NotFuriten) . _handFuriten . snd) $
                      filter ((== Just (-1)) . shantenBy kokushiShanten . (handConcealed %~ cons t) . snd) otherHands
    if null kokushiWins -- chankan on ankan only when kokushi could win from it
        then return $ WaitingDraw pk True
        else return $ WaitingShouts (setFromList $ map fst kokushiWins) [] (each._2 .~ Shout Chankan pk t [] $ kokushiWins) True

processShouminkan :: InKyoku m => Kaze -> Tile -> m Machine
processShouminkan pk t = do
    handOf' pk >>= shouminkanOn t >>= updateHand pk
    chankanShouts <- getShouts True t
    if null chankanShouts then return (WaitingDraw pk True)
                          else do tellEvents . map DealWaitForShout =<< toWaitShouts chankanShouts
                                  return (WaitingShouts (setFromList $ map fst chankanShouts) [] chankanShouts True)

checkEndConditions :: InKyoku m => m Machine
checkEndConditions = do
    updateTempFuritens

    tilesLeft      <- use pWallTilesLeft
    dora           <- use pDora
    everyoneRiichi <- use sHands <&> allOf (each.handRiichi) (/= NoRiichi)
    firstDiscards  <- use sHands <&> toListOf (each.handDiscards._head.dcTile)

    let fourWindTiles = length firstDiscards == 4 && isKaze (headEx firstDiscards) && allSame firstDiscards

    case () of
        _ | length dora == 5 -> endKyoku $ DealAbort SuuKaikan -- TODO check that someone is not waiting for the yakuman
          | tilesLeft == 0   -> nagashiOrDraw
          | everyoneRiichi   -> endKyoku $ DealAbort SuuchaRiichi
          | fourWindTiles    -> endKyoku $ DealAbort SuufonRenda
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
    imapM_ (\pk (player,_,_) -> tellEvent . DealStarts $ PlayerKyoku player pk deal) (deal^.pPlayers)

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
        allHandsInterrupted <- use sHands <&> allOf (each.handFlags) (notMember HandFirsRoundUninterrupted)
        when allHandsInterrupted $ unsetFlag FirstRoundUninterrupted

    updateHand pk =<< (if wanpai then drawDeadWall else drawWall) =<< handOf' pk
    tellEvent $ DealTurnAction pk $ TurnTileDraw wanpai Nothing

drawWall :: InKyoku m => Hand -> m Hand
drawWall hand = do
    wallHead <- preuse (sWall._head) >>= maybe (throwError "Wall is empty") return
    sWall %= tailEx
    wallHead `toHand` hand

drawDeadWall :: InKyoku m => Hand -> m Hand
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

    shouts <- getShouts False (d^.dcTile)
    waiting <- toWaitShouts shouts
    tellEvents $ map DealWaitForShout waiting
    return $ if null waiting
                 then CheckEndConditionsAfterDiscard
                 else WaitingShouts (setFromList $ map fst shouts) [] shouts False

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
    handOf' pk >>= \h -> case h^.handFuriten of
        NotFuriten | tile `elem` handGetAgari h -> updateHand pk $ h & handFuriten .~ TempFuriten
        TempFuriten                             -> updateHand pk $ h & handFuriten .~ NotFuriten
        _                                       -> return ()
    forM_ (L.delete pk [Ton .. Pei]) $ \k -> handOf' k >>= \h -> case h^.handFuriten of
        NotFuriten | tile `elem` handGetAgari h -> updateHand k $ h & handFuriten .~ TempFuriten
        _                                       -> return ()

-- ** Turn-passing

-- | Next player who draws a tile
advanceTurn :: InKyoku m => m Kaze
advanceTurn = do
    pk <- use pTurn <&> succCirc
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
    ura <- use (sWanpai.wUraDora) <&> take count
    tellEvent $ DealFlipDora ura
    setFlag . OpenedUraDora $ map TileEq ura

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
processShouts []                  _      = return $ CheckEndConditionsAfterDiscard
processShouts shouts@((fsk,fs):_) _chank = do
    tk <- use pTurn
    th <- handOf' tk

    (th':_) <- forM shouts $ \(sk, s) -> do
        sh <- handOf' sk
        th' <- shoutFromHand sk s th
        sh' <- if null (shoutTo s) then rons s sh else meldTo s sh -- if null then kokushi. can't use a mentsu 'cause mentsu have >=2 tiles
        updateHand sk sh'
        tellEvent $ DealTurnShouted sk s
        return th'

    updateHand tk th'

    case () of
        _ | shoutKind fs `elem` [Ron, Chankan] -> endRon (map fst shouts) tk >>= endKyoku
          | shoutKind fs == Kan -> unsetFlag FirstRoundUninterrupted >> return (WaitingDraw fsk True)
          | otherwise -> do
              unsetFlag FirstRoundUninterrupted
              tellEvent $ DealTurnBegins fsk
              return (WaitingDiscard fsk)

-- ** Ending

endKyoku :: InKyoku m => KyokuResults -> m Machine
endKyoku res = return (KyokuEnded res)

endGame :: InKyoku m => FinalPoints -> m Machine
endGame points = do tellEvent $ GameEnded points
                    return $ HasEnded points

endTsumo :: InKyoku m => Kaze -> m KyokuResults
endTsumo winner = do
    handOf' winner >>= updateHand winner . setAgariTsumo

    vh     <- getValuedHand winner
    honba  <- use pHonba
    riichi <- use pRiichi
    payers <- use pPlayers <&> tsumoPayers honba (vh^.vhValue.vaValue) . L.delete winner . map fst . itoList

    let win = (winner, - sumOf (each._2) payers + riichi, vh)

    dealEnds $ DealTsumo [win] payers

endDraw :: InKyoku m => m KyokuResults
endDraw = do
    hands <- use sHands
    let x@(tenpaiHands, nootenHands) = partition (tenpai.snd) (itoList hands) -- & both.each %~ fst
        (receive, pay) | null tenpaiHands || null nootenHands = (0, 0)
                       | otherwise                            = x & both %~ div 3000.fromIntegral.length
    dealEnds $ DealDraw (map (\(k,h) -> (k,receive,h^.handCalled,h^.handConcealed)) tenpaiHands) (map ((,-pay) . fst) nootenHands)

endRon :: InKyoku m => [Kaze] -> Kaze -> m KyokuResults
endRon winners payer = do
    valuedHands <- mapM getValuedHand winners
    honba       <- use pHonba
    riichi      <- use pRiichi

    let pointsFromPayer  = zipWith valuedHandPointsForRon winners valuedHands
        pointsRiichi     = riichi
        pointsHonba      = honba * 300
        Just extraGoesTo = payer ^.. iterated succCirc & find (`elem` winners) -- Iterate every kaze, so one must be an element. unless no one won.
        winEntries       = map (\x -> if x^._1 == extraGoesTo then x & _2 +~ pointsHonba + pointsRiichi else x) $ zip3 winners pointsFromPayer valuedHands

    dealEnds $ DealRon winEntries [(payer, negate $ pointsHonba + sum pointsFromPayer)]

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
    revealUraDora -- TODO wrong place for this
    vh <- valueHand pk <$> handOf' pk <*> use id
    when (length (vh^..vhValue.vaYaku.each.filtered yakuNotExtra) == 0) $
        throwError $ "Need at least one yaku that ain't dora to win.\nThe ValuedHand: " ++ tshow vh
    return vh

-- ** Scoring

payPoints :: KyokuResults -> [GameEvent]
payPoints res = case res of
    DealAbort{}  -> []
    DealDraw{..} -> map (uncurry GamePoints) $ map (\x -> (x^._1,x^._2)) dTenpais ++ dNooten
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
    tellEvent . DealStarts $ PlayerKyoku p pk deal

updatePlayerNick :: InKyoku m => Player -> Text -> m ()
updatePlayerNick p nick = playerToKaze p >>= \pk -> tellEvent (DealNick pk p nick)

-- | Set the hand of player
updateHand :: InKyoku m => Kaze -> Hand -> m ()
updateHand pk new = do
    p   <- kazeToPlayer pk
    sHands.at pk .= Just new
    tellEvent (DealPrivateHandChanged p pk new)
    tellEvent (DealPublicHandChanged pk $ PlayerHand new)

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

handOf' :: InKyoku m => Kaze -> m Hand
handOf' p = use (handOf p) >>= maybe (throwError "handOf': Player not found") return

----------------------------------------------------------------------------------------

-- * Waiting

-- | Get all possible shouts from all players for a given tile.
filterCouldShout :: Tile            -- ^ Tile to shout
                 -> Kaze            -- ^ Whose tile
                 -> Map Kaze Hand  -- ^ All hands
                 -> [(Kaze, Shout)] -- ^ Sorted in correct precedence (highest priority as head)
filterCouldShout dt np = sortBy (shoutPrecedence np)
    . concatMap flatten . Map.toList
    . Map.mapWithKey (shoutsOn np dt)
    . Map.filter ((== NotFuriten) . _handFuriten) -- discard furiten
  where flatten (k, xs) = map (k,) xs

-- | Flag for chankan.
getShouts :: InKyoku m => Bool -> Tile -> m [(Kaze, Shout)]
getShouts chankan dt = do
    lastTile <- use pWallTilesLeft <&> (== 0)
    shouts   <- filterCouldShout dt <$> use pTurn <*> use sHands
    filterM couldContainYaku $
        if' chankan (over (each._2) toChankan . filter ((== Ron) . shoutKind.snd)) id $
        if' lastTile (filter $ (== Ron) . shoutKind . snd) id -- when there are no tiles, only go-out shouts are allowed
        shouts

yakuNotExtra :: Yaku -> Bool
yakuNotExtra Yaku{}      = True
yakuNotExtra YakuExtra{} = False

toChankan :: Shout -> Shout
toChankan s = s { shoutKind = Chankan }

-- | The shout wouldn't result in a 0-yaku mahjong call
couldContainYaku :: InKyoku m => (Kaze, Shout) -> m Bool
couldContainYaku (k, s)
    | Ron <- shoutKind s = do
        h  <- handOf' k
        vh <- valueHand k <$> meldTo s h <*> get
        return $ length (vh^..vhValue.vaYaku.each.filtered yakuNotExtra) /= 0
    | otherwise = return True

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
