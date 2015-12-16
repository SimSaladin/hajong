{-# LANGUAGE LambdaCase #-}
------------------------------------------------------------------------------
-- | 
-- Module         : MahjongTest.Mechanics
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module MahjongTest.Mechanics where

import Prelude hiding (Discard)
import Control.Monad.State (runStateT)
import Mahjong
import Mahjong.Hand as Hand

import qualified Data.Map as M

tests :: TestTree
tests = testGroup "Game mechanics"
  [ gameFlowTests, furitenTests, riichiTests, weirdYaku, yakumans, scoringTests ]

gameFlowTests :: TestTree
gameFlowTests = testGroup "Game flow" 
  [ testCase "Game ends when a Ron is called" $ do
        kyoku <- testKyoku <&> sHands.ix Nan .handConcealed . _Wrapped .~ ["M5", "M5", "M5", "P5", "P6", "P7", "P8", "P8", "S4", "S5", "S6", "S7", "S8"]
                           <&> sHands.ix Ton .handConcealed . _Wrapped .~ ["S3"]
        let res = runKyokuState kyoku $ do
                stepped_ InpAuto
                stepped_ $ InpTurnAction Ton $ TurnTileDraw False Nothing
                stepped_ $ InpTurnAction Ton $ TurnTileDiscard $ Mahjong.Discard "S3" Nothing False
                stepped $ InpShout Nan $ Shout Ron Ton "S3" ["S4", "S5"]
        requireRight res $ \case
            (_, (KyokuEnded results,_)) -> return ()
            (_, (m,_))                  -> assertFailure $ "Expected 'Ended' but received " <> show m

  , testCase "Discarding a tile results in correct state and yields correct GameEvents" $ do
        kyoku <- testKyoku
        let Just tile = kyoku ^? sHands . ix Ton . handConcealed . _Wrapped . _head
            res = runKyokuState kyoku $ do
                stepped_ InpAuto
                stepped_ $ InpTurnAction Ton $ TurnTileDraw False Nothing
                stepped $ InpTurnAction Ton $ TurnTileDiscard $ Mahjong.Discard tile Nothing False
        requireRight res $ \(_, (_, k)) -> k ^?! sHands . ix Ton . handDiscards == [Hand.Discard tile Nothing False] @? "Discard didn't equal"

  , testCase "New kyoku state has correct properties" $ do
        kyoku <- testKyoku
        kyoku^.sWall.to length == 136-14-4*13 @? "Wall of 136-14-4*13 tiles"
        kyoku^.sWanpai.to length == 13 @? "Wanpai of 13 (+1 dora) tiles"
        kyoku^.sHands ^.. each.handConcealed._Wrapped.to length == [13, 13, 13, 13]  @? "Four hands of 13 tiles"
        kyoku^.pDora.to length == 1 @? "One dora tile"
        kyoku^.pRound == (Ton, 1) @? "First round was not Ton, 1"

  , testCase "Tsumo is an option in the DealPrivateHandChanged event" $ do
        kyoku <- testKyoku <&> sHands . ix Ton . handConcealed . _Wrapped .~ handThatWinsWithP5
                           <&> sWall %~ ("P5" <|)
        let res = runKyokuState kyoku $ stepped InpAuto >> stepped InpAuto -- start and draw
        requireRight res $ \(evs, (m, k)) -> do
            let check (DealPrivateHandChanged _ _ hand : xs) = assertBool "handCanTsumo was False" (hand^.handCanTsumo._Wrapped)
                check (_ : xs)                               = check xs
                check []                                     = assertFailure "No Dealprivatechanged in events"
                in check evs

  , testCase "Kyoku goes through with consecutive InpAuto actions" $ do
      kyoku <- testKyoku
      case runKyokuState kyoku (replicateM_ 250 $ stepped InpAuto) of
          Left "This kyoku has ended!" -> return ()
          Right _                      -> assertFailure "Kyoku didn't end withing 250 automatic advances"

  , testCase "One han minimum to win a hand" $ do
      undefined
  ]

furitenTests :: TestTree
furitenTests = testGroup "Furiten tests"
  [ testCase "Trying to Ron in furiten results in kyoku level error" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed . _Wrapped .~ handThatWinsWithP5
                         <&> sHands . ix Nan . handDiscards .~ [Mahjong.Discard "P5" Nothing False]
                         <&> sHands . ix Nan . handFuriten .~ return Furiten
                         <&> sWall %~ ("P5" <|)
      let res = runKyokuState kyoku $ do
              stepped_ InpAuto -- start
              stepped_ InpAuto -- draw
              stepped_ InpAuto -- discard P5
              stepped $ InpShout Nan $ Shout Ron Ton "P5" ["P5"] -- should error
      case res of
          Left err -> err @?= "You are furiten"
          x -> assertFailure $ show x
  ]

riichiTests :: TestTree
riichiTests = testGroup "Riichi tests"
  [ testCase "Riichi tiles are calculated correctly in the DealWaitForTurnAction event" $ do
        kyoku <- testKyoku <&> sHands . ix Ton . handConcealed . _Wrapped .~ ["M1", "M2", "M3", "P1", "P2", "P3", "P5", "P7", "P8", "P9", "S2", "S3", "W "]
                           <&> sWall %~ ("S4" <|)

        let res = runKyokuState kyoku $ stepped InpAuto >> stepped InpAuto -- start and draw

        requireRight res $ \(evs, (m, k)) -> do
            let check (DealWaitForTurnAction (_,_,_,rs) : xs) = ["P5", "W "] == rs @? ("Riichi tiles didn't match (" <> show rs <> " vs [P5, W])")
                check (_ : xs)                                = check xs
                check []                                      = assertFailure "No Dealprivatechanged in events"
                in check evs

  , testCase "Riichi points are transferred to table from player(s) on riichi" $ do
      kyoku <- testKyoku <&> sHands . each . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ (["P2", "P2", "P3", "P4"] ++)

      let res = runKyokuState kyoku $ do
              autoAndDiscard Ton $ Discard "P2" Nothing True
              autoAndDiscard Nan $ Discard "P2" Nothing True

      requireRight res $ \(_evs, (m, k)) -> do
          k ^. pRiichi                 @?= 2000
          k ^?! pPlayers . ix Ton . _2 @?= 24000
          k ^?! pPlayers . ix Nan . _2 @?= 24000

  , testCase "Riichi points from table are transferred to winner on ron" $ do
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed . _Wrapped .~ handThatWinsWithP5
                         <&> pRiichi .~ 2000
                         <&> sWall %~ (["P2", "P5"] ++)

      let res = runKyokuState kyoku $ do
            autoAndDiscard Ton $ Discard "P2" Nothing False
            autoAndDiscard Nan $ Discard "P5" Nothing False
            stepped_ $ InpShout Ton $ Shout Ron Nan "P5" ["P5"]

      requireRight res $ \(_evs, (m, k)) -> do
          Right k' <- maybeNextDeal k
          k' ^. pRiichi @?= 0

  , testCase "Hand restarts on four riichi" $ do
      kyoku <- testKyoku <&> sHands . each . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ (["P2", "P2", "P3", "P4"] ++)

      let res = runKyokuState kyoku $ do
              autoAndDiscard Ton  $ Discard "P2" Nothing True
              autoAndDiscard Nan  $ Discard "P2" Nothing True
              autoAndDiscard Shaa $ Discard "P3" Nothing True
              autoAndDiscard Pei  $ Discard "P4" Nothing True
              stepped_ InpAuto -- check end conditions

      requireRight res $ \(_evs, (r,k)) -> do
          r @?= KyokuEnded (DealAbort SuuchaRiichi)
          k ^. pRiichi @?= 4000

  , testCase "Riichi not possible if under 1000 points" $ do
      kyoku <- testKyoku <&> sHands . each . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> pPlayers . ix Ton . _2 .~ 500
                         <&> sWall %~ (["P2", "P2", "P3", "P4"] ++)
      let res = runKyokuState kyoku $ do
              stepped_ InpAuto -- start
              stepped_ InpAuto -- draw
              stepped_ $ InpTurnAction Ton $ TurnTileDiscard $ Discard "P2" Nothing True
      case res of
          Left err -> err @?= "Cannot riichi: not enough points"
          Right _  -> assertFailure "Should have errored"
  ]

weirdYaku :: TestTree
weirdYaku = testGroup "Yaku dependant on the whole game state"
  [ testCase "Nagashi mangan is yielded" $ do
      kyoku <- testKyoku <&> pWallTilesLeft .~ 1
                         <&> sWall %~ ("W " <|)
                         <&> sHands . ix Nan . handDiscards .~ [Mahjong.Discard "P5" Nothing False]
                         <&> sHands . ix Shaa . handDiscards .~ [Mahjong.Discard "P5" Nothing False]
                         <&> sHands . ix Pei . handDiscards .~ [Mahjong.Discard "P5" Nothing False]
      let res = runKyokuState kyoku $ do
            stepped_ InpAuto
            stepped_ InpAuto -- draw
            stepped_ InpAuto -- discard
            stepped_ InpAuto -- continue
      case res of
          Right (_, (KyokuEnded (DealTsumo{}),_)) -> return ()
          x -> error $ show x

  , testCase "Chankan is possible" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sHands . ix Ton . handCalled .~ [Mentsu Koutsu "P5" (Just (Shout Pon Shaa "P5" ["P5", "P5"]))]
                         <&> sWall %~ ("P5" <|)

      let res = runKyokuState kyoku $ do
            stepped_ InpAuto
            stepped_ InpAuto -- draw
            stepped_ $ InpTurnAction Ton $ TurnShouminkan "P5"
            stepped $ InpShout Nan $ Shout Chankan Ton "P5" ["P5"]

      requireRight res $ \case
          (_, (KyokuEnded (DealRon [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 1 "Chankan"]
          x -> assertFailure (show x)
  ]

yakumans :: TestTree
yakumans = testGroup "Yakumans that are dependant on the whole game state"
  [ testCase "Tenhou (dealer goes out on first draw)" $ do
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ ("P5" <|)

      let res = runKyokuState kyoku $ do
            stepped_ InpAuto -- start
            stepped_ InpAuto -- draw
            stepped $ InpTurnAction Ton $ TurnTsumo

      requireRight res $ \case
          (_, (KyokuEnded (DealTsumo [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 13 "Tenhou", Yaku 1 "Menzen Tsumo"]
          x -> assertFailure (show x)

  , testCase "Chiihou (non-dealer goes out on first draw uninterrupted)" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ ("P1" <|) . ("P5" <|)

      let res = runKyokuState kyoku $ do
            stepped_ InpAuto -- start
            stepped_ InpAuto -- draw Ton
            stepped_ InpAuto -- discard Ton
            stepped_ InpAuto -- ignore shout wait
            stepped_ InpAuto -- check end conditions
            stepped_ InpAuto -- draw Nan
            stepped $ InpTurnAction Nan $ TurnTsumo

      requireRight res $ \case
          (_, (KyokuEnded (DealTsumo [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 13 "Chiihou", Yaku 1 "Menzen Tsumo"]
          x -> assertFailure (show x)

 {-
  , testCase "Renhou (out on first round uninterrupted)" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ ("P5" <|)

      let res = runKyokuState kyoku $ do
            stepped_ InpAuto -- start
            stepped_ InpAuto -- draw
            stepped_ InpAuto -- discard
            stepped $ InpShout Nan $ Shout Ron Ton "P5" ["P5"]

      requireRight res $ \case
          (_, (KyokuEnded (DealRon [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 13 "Renhou"]
          x -> assertFailure (show x)
          -}
  ]

scoringTests :: TestTree
scoringTests = testGroup "Scoring"
    [ testCase "Honba is deducted from payers and added to winner on tsumo" $ do
        kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ handThatWinsWithP5
                           <&> sWall %~ ("P5" <|)
                           <&> pFlags .~ mempty
                           <&> pHonba .~ 1

        let res = runKyokuState kyoku $ do
                stepped_ InpAuto -- start
                stepped_ InpAuto -- draw
                stepped_ $ InpTurnAction Ton $ TurnTsumo

        requireRight res $ \(_evs, (KyokuEnded r, _k)) -> do
            headEx (dWinners r) ^. _2 @?= 1200 + 300
            dPayers r ^..each._2 @?= [-400 - 100, -400 - 100, -400 - 100]

    , testCase "Honba is deducted from payer and added to winner on ron" $ do
        kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ handThatWinsWithP5Pinfu
                           <&> sWall %~ ("P5" <|)
                           <&> pHonba .~ 2

        let res = runKyokuState kyoku $ do
                stepped_ InpAuto -- start
                stepped_ InpAuto -- draw
                stepped_ InpAuto -- discard
                stepped_ $ InpShout Nan $ Shout Ron Ton "P5" ["P4", "P3"]

        requireRight res $ \(_evs, (KyokuEnded r, _k)) -> do
            headEx (dWinners r) ^. _2 @?= 1000 + 2 * 300
            dPayers r ^..each._2 @?= [-1000 - 2 * 300]
    ]
    -- TODO: multi-ron

-- agari to pair
handThatWinsWithP5 :: [Tile]
handThatWinsWithP5      = ["M1", "M2", "M3", "P1", "P2", "P3", "P5", "P7", "P8", "P9", "S2", "S3", "S4"]

-- also P2
handThatWinsWithP5Pinfu :: [Tile]
handThatWinsWithP5Pinfu = ["M1", "M2", "M3", "P3", "P4", "P5", "P5", "P7", "P8", "P9", "S2", "S3", "S4"]

testKyoku = newKyoku fourPlayers (map tshow [1..4])

runKyokuState k ma = runStateT ma (NotBegun 0, k)

requireRight (Left err) go = assertFailure (unpack err)
requireRight (Right res) go = go res

autoAndDiscard k d = go where
  go = do st <- steppedSt InpAuto
          case st of
            WaitingDiscard{} -> stepped_ (InpTurnAction k $ TurnTileDiscard d)
            _                -> go

steppedSt step = stepped_ step >> fmap fst get

stepped s = do (m, k) <- get
               (m, k, xs) <- lift $ runKyoku k (step m s)
               put (m, k)
               return xs
stepped_ s = stepped s >> return ()
