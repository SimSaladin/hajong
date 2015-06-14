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

import Control.Monad.State (runStateT)
import Mahjong
import Mahjong.Hand as Hand

import qualified Data.Map as M

tests :: TestTree
tests = testGroup "Game mechanics"
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

  , testCase "Can tsumo when picked" $ do
        kyoku <- testKyoku <&> sHands . ix Ton . handConcealed . _Wrapped .~ handThatWinsWithP5
                           <&> sWall %~ ("P5" <|)
        let res = runKyokuState kyoku $ stepped InpAuto >> stepped InpAuto -- start and draw
        requireRight res $ \(evs, (m, k)) -> do
            let check (DealPrivateHandChanged _ _ hand : xs) = assertBool "handCanTsumo was False" (hand^.handCanTsumo._Wrapped)
                check (_ : xs)                               = check xs
                check []                                     = assertFailure "No Dealprivatechanged in events"
                in check evs

  , testCase "Can riichi when picked" $ do
        kyoku <- testKyoku <&> sHands . ix Ton . handConcealed . _Wrapped .~ ["M1", "M2", "M3", "P1", "P2", "P3", "P5", "P7", "P8", "P9", "S2", "S3", "W "]
                         <&> sWall %~ ("S4" <|)
        let res = runKyokuState kyoku $ stepped InpAuto >> stepped InpAuto -- start and draw
        requireRight res $ \(evs, (m, k)) -> do
            let check (DealWaitForTurnAction (_,_,_,rs) : xs) = ["P5", "W "] == rs @? ("Riichi tiles didn't match (" <> show rs <> " vs [P5, W])")
                check (_ : xs)                                = check xs
                check []                                      = assertFailure "No Dealprivatechanged in events"
                in check evs

  , testCase "Kyoku goes through with consecutive InpAuto actions" $ do
      kyoku <- testKyoku
      case runKyokuState kyoku (replicateM_ 250 $ stepped InpAuto) of
          Left "This kyoku has ended!" -> return ()
          Right _                      -> assertFailure "Kyoku didn't end withing 250 automatic advances"

  , testCase "Trying to Ron in furiten results in kyoku level error" $ do
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

  , testCase "Nagashi mangan is yielded" $ do
      kyoku <- testKyoku <&> pWallTilesLeft .~ 1
                         <&> sWall %~ ("W " <|)
                         <&> sHands . ix Nan . handDiscards .~ [Mahjong.Discard "P5" Nothing False]
                         <&> sHands . ix Shaa . handDiscards .~ [Mahjong.Discard "P5" Nothing False]
                         <&> sHands . ix Pei . handDiscards .~ [Mahjong.Discard "P5" Nothing False]
      let res = runKyokuState kyoku $ do
            stepped_ InpAuto
            stepped_ InpAuto -- draw
            stepped_ InpAuto -- discard
            xs <- stepped InpAuto -- continue
            when (not $ null xs) $ stepped_ InpAuto -- if there were shouts, go forward
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

  , testCase "Tenhou (dealer goes out on first draw)" $ do
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
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ ("P5" <|) . ("P5" <|)
      let res = runKyokuState kyoku $ do
            stepped_ InpAuto -- start
            stepped_ InpAuto -- draw Ton
            stepped_ InpAuto -- discard Ton
            stepped_ InpAuto -- ignore shout wait
            stepped_ InpAuto -- draw Nan
            stepped $ InpTurnAction Ton $ TurnTsumo
      requireRight res $ \case
          (_, (KyokuEnded (DealTsumo [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 13 "Chiihou"]
          x -> assertFailure (show x)
  ]

-- agari to pair
handThatWinsWithP5 :: [Tile]
handThatWinsWithP5 = ["M1", "M2", "M3", "P1", "P2", "P3", "P5", "P7", "P8", "P9", "S2", "S3", "S4"]

testKyoku = newKyoku fourPlayers (map tshow [1..4])

runKyokuState k a = runStateT a (NotBegun, k)

requireRight (Left err) go = assertFailure (unpack err)
requireRight (Right res) go = go res

stepped s = do (m, k) <- get
               (m, k, xs) <- lift $ runKyoku k (step m s)
               put (m, k)
               return xs
stepped_ s = stepped s >> return ()
