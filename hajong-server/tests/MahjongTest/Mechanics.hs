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
  [ gameFlowTests, furitenTests, riichiTests, weirdYaku, yakumans ]

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
        kyoku^.pRound == Ton @? "First round was not Ton"

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
          Left err -> err @=? "You are furiten"
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

  , testCase "Riichi points are transferred to table from player on riichi, not given on draw and given on win" $ do
      kyoku <- testKyoku <&> sHands . each . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ (["P2", "P2", "P3", "P4"] ++)
      let res = runKyokuState kyoku $ do
              stepped_ InpAuto -- start
              stepped_ InpAuto -- draw
              stepped_ $ InpTurnAction Ton $ TurnTileDiscard $ Discard "P2" Nothing True
              stepped_ InpAuto -- continue
              stepped_ InpAuto -- draw
              stepped_ $ InpTurnAction Nan $ TurnTileDiscard $ Discard "P2" Nothing True
      requireRight res $ \(_evs, (m, k)) -> do
          k^.pRiichi @=? 2000
          k^.pPlayers.ix Ton. _2 @=? 24000
          k^.pPlayers.ix Nan. _2 @=? 24000

  , testCase "Hand restarts on four riichi" $ do
      kyoku <- testKyoku <&> sHands . each . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ (["P2", "P2", "P3", "P4"] ++)
      let res = runKyokuState kyoku $ do
              let startDrawDiscard k t = do
                    stepped_ InpAuto >> stepped_ InpAuto >> stepped_ (InpTurnAction k $ TurnTileDiscard t Nothing True)
              startDrawDiscard Ton "P2"
              startDrawDiscard Nan "P2"
              startDrawDiscard Shaa "P3"
              startDrawDiscard Pei "P4"
              stepped_ InpAuto
      requireRight res $ \(_evs, (r,k)) -> r @=? KyokuEnded (DealAbort SuuchaRiichi)

  , testCase "Riichi not possible if under 1000 points" $ do
      kyoku <- testKyoku <&> sHands . each . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> pPlayers.ix Ton._2 .~ 500
                         <&> sWall %~ (["P2", "P2", "P3", "P4"] ++)
      let res = runKyokuState kyoku $ do
              stepped_ InpAuto -- start
              stepped_ InpAuto -- draw
              stepped_ $ InpTurnAction Ton $ TurnTileDiscard $ Discard "P2" Nothing True
      case res of
          Left err -> err @=? "Can't riichi with under 1000 points"
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
                         <&> sHands . ix Ton . handConcealed._Wrapped .~ ["P5", "P5", "P5", "P5", "S1"]
      let res = runKyokuState kyoku $ do
            stepped_ InpAuto
            stepped_ InpAuto
            stepped_ $ InpTurnAction Ton $ TurnAnkan "P5"
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
          (_, (KyokuEnded (DealTsumo [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 13 "Tenhou"]
          x -> assertFailure (show x)

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

  , testCase "Chiihou (non-dealer goes out on first draw uninterrupted)" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ ("P1" <|) . ("P5" <|)
      let res = runKyokuState kyoku $ do
            stepped_ InpAuto -- start
            stepped_ InpAuto -- draw Ton
            stepped_ InpAuto -- discard Ton
            stepped_ InpAuto -- ignore shout wait
            stepped_ InpAuto -- draw Nan
            stepped $ InpTurnAction Nan $ TurnTsumo
      requireRight res $ \case
          (_, (KyokuEnded (DealTsumo [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 13 "Chiihou"]
          x -> assertFailure (show x)
  ]

-- agari to pair
handThatWinsWithP5 :: [Tile]
handThatWinsWithP5 = ["M1", "M2", "M3", "P1", "P2", "P3", "P5", "P7", "P8", "P9", "S2", "S3", "S4"]

testKyoku = newKyoku fourPlayers (map tshow [1..4])

runKyokuState k ma = runStateT ma (NotBegun 0, k)

requireRight (Left err) go = assertFailure (unpack err)
requireRight (Right res) go = go res

stepped s = do (m, k) <- get
               (m, k, xs) <- lift $ runKyoku k (step m s)
               put (m, k)
               return xs
stepped_ s = stepped s >> return ()
