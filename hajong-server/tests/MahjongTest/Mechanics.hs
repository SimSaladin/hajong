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
  [ testCase "Game ends when a Ron is called and game is continued" $ do

        kyoku <- testKyoku <&> sHands.ix Nan .handConcealed . _Wrapped .~ ["M5", "M5", "M5", "P5", "P6", "P7", "P8", "P8", "S4", "S5", "S6", "S7", "S8"]
                           <&> sHands.ix Ton .handConcealed . _Wrapped .~ ["S3"]

        let res = runKyokuState kyoku $ do
                drawAndTurnAction Ton $ TurnTileDiscard $ Mahjong.Discard "S3" Nothing False
                stepped $ InpShout Nan $ Shout Ron Ton "S3" ["S4", "S5"]
                autoEndTurn

        requireRight res $ \case
            (_, (KyokuEnded results,_)) -> return ()
            (_, (m,_))                  -> assertFailure $ "Expected 'Ended' but received " <> show m

  , testCase "Discarding a tile results in correct state and yields correct GameEvents" $ do
        kyoku <- newKyoku fourPlayers (map tshow [1..4])
        let Just tile = kyoku ^? sHands . ix Ton . handConcealed . _Wrapped . _head
            res = runKyokuState kyoku $ do
                stepped_ InpAuto
                stepped_ $ InpTurnAction Ton $ TurnTileDraw False Nothing
                stepped $ InpTurnAction Ton $ TurnTileDiscard $ Mahjong.Discard tile Nothing False
        requireRight res $ \(_, (_, k)) -> k ^?! sHands . ix Ton . handDiscards.to length == 1 @? "Discard didn't equal"

  , testCase "New kyoku state has correct properties" $ do
        kyoku <- newKyoku fourPlayers (map tshow [1..4])
        kyoku^.sWall.to length                   @?= 136-14-4*13
        kyoku^.sWanpai.to wanpaiTiles.to length  @?= 13 -- + 1 x dora
        kyoku^.pDora.to length                   @?= 1
        kyoku^.pRound                            @?= (Ton, 1)
        length (kyokuTiles kyoku)                @?= 136
        length (filter isAka $ kyokuTiles kyoku) @?= 4
        kyoku^.sHands ^.. each.handConcealed._Wrapped.to length @?= [13, 13, 13, 13]

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

  , testCase "One han minimum to win a hand, not counting YakuExtra" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed . _Wrapped .~ handThatWinsWithP5
                         <&> pDora .~ ["P5"]
                         <&> sWall %~ ("P5" <|)
                         <&> pFlags .~ setFromList [] -- not first round

      let res = runKyokuState kyoku $ do
            autoAndDiscard Ton $ Discard "P5" Nothing False
            stepped $ InpShout Nan $ Shout Ron Ton "P5" ["P5"]
            autoEndTurn
        
      case res of
          Right (_evs, (r, _k)) -> assertFailure $ "Didn't fail, received " ++ show r
          Left r                -> return ()

  , testCase "Dora case test: flipping and winning" $ do
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ (replicate 3 "M1" ++ replicate 3 "M2" ++ replicate 3 "M3" ++ replicate 3 "M9" ++ ["S1"])
                         <&> sWall %~ (["M1", "M3"] ++)
                         <&> sWanpai.wSupplement .~ ["M2", "P1", "S1"]
                         <&> sWanpai.wUraDora .~ ["P1", "P2", "P3", "P4", "P5"]

      let res = runKyokuState kyoku $ do
              drawAndTurnAction Ton $ TurnAnkan "M1"
              drawAndTurnAction Ton $ TurnAnkan "M2"
              autoAndDiscard Ton $ Discard "P1" Nothing False
              autoAndDiscard Nan $ Discard "M3" Nothing False
              stepped $ InpShout Ton $ Shout Kan Nan "M3" ["M3", "M3", "M3"]
              drawAndTurnAction Ton TurnTsumo

      requireRight res $ \(_evs, (_r,k)) -> do
          k ^. pDora.to length @?= 4
          k ^. pFlags @?= setFromList [OpenedUraDora $ map TileEq ["P1", "P2", "P3", "P4"]]

  , testCase "Suufonrenda: four consecutive wind discards goes through" $ do
      kyoku <- testKyoku <&> sWall %~ (["W", "W", "W", "W"] ++)

      let res = runKyokuState kyoku $ do
            autoAndDiscard Ton $ Discard "W" Nothing False
            autoAndDiscard Nan $ Discard "W" Nothing False
            autoAndDiscard Shaa $ Discard "W" Nothing False
            autoAndDiscard Pei $ Discard "W" Nothing False
            autoEndTurn

      requireRight res $ \(_evs, (r,_k)) -> r @?= KyokuEnded (DealAbort SuufonRenda)

  , testCase "Suukaikan: four kans called all not by one player" $ do
      kyoku <- testKyoku <&> sWall %~ (["M1", "M2", "M3", "M4"] ++)
                         <&> sHands . ix Ton  . handConcealed._Wrapped %~ (["M1", "M1", "M1"] ++)
                         <&> sHands . ix Nan  . handConcealed._Wrapped %~ (["M2", "M2", "M2"] ++)
                         <&> sHands . ix Shaa . handConcealed._Wrapped %~ (["M3", "M3", "M3"] ++)
                         <&> sHands . ix Pei  . handConcealed._Wrapped %~ (["M4", "M4", "M4", "N"] ++)

      let res = runKyokuState kyoku $ do
            drawAndTurnAction Ton  (TurnAnkan "M1") >> stepped_ InpAuto >> stepped_ InpAuto
            drawAndTurnAction Nan  (TurnAnkan "M2") >> stepped_ InpAuto >> stepped_ InpAuto
            drawAndTurnAction Shaa (TurnAnkan "M3") >> stepped_ InpAuto >> stepped_ InpAuto
            drawAndTurnAction Pei  (TurnAnkan "M4")
            autoEndTurn

      requireRight res $ \(_evs, (r,_k)) -> r @?= KyokuEnded (DealAbort SuuKaikan)

  , testCase "Suukantsu: yakuman" $ do
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ (replicate 4 "M1" ++ replicate 3 "M2" ++ replicate 3 "M3" ++ replicate 3 "M9")
                         <&> sWall %~ (["S1"] ++)
                         <&> sWanpai.wSupplement .~ ["M2", "M3", "M9", "S1"]
                         <&> sWanpai.wUraDora .~ []
                         <&> pFlags .~ mempty
                         <&> pDora .~ []

      let res = runKyokuState kyoku $ do
              drawAndTurnAction Ton $ TurnAnkan "M1"
              drawAndTurnAction Ton $ TurnAnkan "M2"
              drawAndTurnAction Ton $ TurnAnkan "M3"
              drawAndTurnAction Ton $ TurnAnkan "M9"
              stepped_ InpAuto
              drawAndTurnAction Ton TurnTsumo

      requireRight res $ \(_evs, (r,_k)) -> case r of
        KyokuEnded (DealTsumo [win] _) -> do Yaku 13 "Suu Kantsu" `elem` (win^._3.vhValue.vaYaku) @? "Suu kantsu was not yielded"
                                             win^._3.vhValue.vaNamed @?= Just "Yakuman"
        _                              -> assertFailure $ "Expected KyokuEnded but received " ++ show r

  , testCase "Ankan may be robbed to kokushi" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ ["P1", "P9", "M1", "M9", "S1", "S9", "E", "S", "W", "N", "G", "G", "W!"] -- Win on "R"
                         <&> sHands . ix Ton . handConcealed._Wrapped .~ ["R", "R", "R", "P1", "P2"]
                         <&> sWall %~ (["R"] ++)

      let res = runKyokuState kyoku $ do
              drawAndTurnAction Ton $ TurnAnkan "R"
              stepped_ $ InpShout Nan $ Shout Chankan Ton "R" []
              autoEndTurn

      requireRight res $ \_ -> return ()

  , testCase "Ron is not possible if furiten; must fail before shout goes through" $ do
      kyoku <- testKyoku <&> sHands.ix Nan .handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sHands.ix Nan .handFuriten._Wrapped .~ Furiten
                         <&> sHands.ix Shaa .handConcealed._Wrapped .~ handThatWinsWithP5 -- needed to ensure some other possible shout
                         <&> sWall %~ cons "P5"

      let res = runKyokuState kyoku $ do
            autoAndDiscard Ton $ Discard "P5" Nothing False
            stepped_ $ InpShout Nan $ Shout Ron Ton "P5" ["P5"]
            -- must be in waiting state here!

      case res of
          Left err -> err @?= "You are furiten"
          _        -> assertFailure "Game should have errored"

  , testCase "Ron is not possible if 0 yaku; must fail before shout goes through" $ do
      kyoku <- testKyoku <&> sHands.ix Nan .handConcealed . _Wrapped .~ handThatWinsWithP5
                         <&> sHands.ix Nan .handRiichi .~ NoRiichi
                         <&> sHands.ix Shaa .handConcealed._Wrapped .~ handThatWinsWithP5 -- needed to ensure some other possible shout
                         <&> sWall %~ cons "P5"

      let res = runKyokuState kyoku $ do
            autoAndDiscard Ton $ Discard "P5" Nothing False
            stepped_ $ InpShout Nan $ Shout Ron Ton "P5" ["P5"]
            -- must be in waiting state here!

      case res of
          Left err -> err @?= "Need yaku to win"
          _        -> assertFailure "Game should have errored"

  ]

furitenTests :: TestTree
furitenTests = testGroup "Furiten tests"
  [ testCase "Trying to Ron in furiten results in kyoku level error" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed . _Wrapped .~ handThatWinsWithP5
                         <&> sHands . ix Nan . handDiscards .~ [Mahjong.Discard "P5" Nothing False]
                         <&> sHands . ix Nan . handFuriten._Wrapped .~ Furiten
                         <&> sWall %~ ("P5" <|)
      let res = runKyokuState kyoku $ do
              autoAndDiscard Ton $ Discard "P5" Nothing False
              stepped $ InpShout Nan $ Shout Ron Ton "P5" ["P5"] -- should error
              autoEndTurn
      case res of
          Left err -> err @?= "You are furiten"
          x -> assertFailure $ show x

  , testCase "Temporary furiten is effective until next own draw" $ do
      undefined

  , testCase "Chiitoi furiten" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ ["M2", "M2", "M5", "M5", "M7", "M7", "P3", "P3", "M9", "M9", "M1", "M1", "M3"]
                         <&> sHands . ix Nan . handDiscards .~ [Discard "M3" Nothing False]
                         <&> sHands . ix Nan %~ updateFuriten
                         <&> sWall %~ ("M3" <|)

      let res = runKyokuState kyoku $ do
            autoAndDiscard Ton $ Discard "M3" Nothing False
            stepped_ $ InpShout Nan $ Shout Ron Ton "M3" ["M3"]
            autoEndTurn

      case res of
          Left err -> err @?= "You are furiten"
          Right x  -> assertFailure "Should have errored"

  , testCase "Kokushi furiten" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ ["P1", "P9", "M1", "M9", "S1", "S9", "E", "S", "W", "N", "G", "R", "W!"]
                         <&> sHands . ix Nan . handDiscards .~ [Discard "G" Nothing False]
                         <&> sHands . ix Nan %~ updateFuriten
                         <&> sWall %~ ("G" <|)

      let res = runKyokuState kyoku $ do
            autoAndDiscard Ton $ Discard "G" Nothing False
            stepped_ $ InpShout Nan $ Shout Ron Ton "G" []
            autoEndTurn

      case res of
          Left err -> err @?= "You are furiten"
          Right x  -> assertFailure "Should have errored"
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
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed . _Wrapped .~ handThatWinsWithP5Pinfu
                         <&> pRiichi .~ 2000
                         <&> sWall %~ (["P3", "P5"] ++)

      let res = runKyokuState kyoku $ do
            autoAndDiscard Ton $ Discard "P3" Nothing False
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

  , testCase "Ankan is possible in riichi when it doesn't change wait" $ do
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ (["M1"] ++)

      let res = runKyokuState kyoku $ do
              drawAndTurnAction Ton $ TurnAnkan "M1"

      requireRight res $ \(_evs, (_r,_k)) -> do
          return ()

  , testCase "Ankan is not possible in riichi when it would change waits" $ do
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ ["M1", "M1", "M2", "M3", "P1", "P2", "P3", "P7", "P8", "P9", "S2", "S3", "S4"]
                         <&> sWall %~ (["M1"] ++)

      let res = runKyokuState kyoku $ do
              drawAndTurnAction Ton $ TurnAnkan "M1"

      case res of
          Left err -> return ()
          Right _ -> assertFailure "Game should have errored"

  , testCase "(tenpai test) player is not tenpai if waiting only on a tile he already has four of" $ do
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ ["M1", "M1", "M1", "M1", "P1", "P2", "P3", "P7", "P8", "P9", "S2", "S3", "S4"]
                         <&> sWall %~ (["W"] ++)

      let res = runKyokuState kyoku $ do
              drawAndTurnAction Ton $ TurnTileDiscard $ Discard "W" Nothing True

      case res of
          Left err -> return ()
          Right _ -> assertFailure "Game should have errored"
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
                         <&> sHands . ix Ton . handCalled .~ [fromShout $ Shout Pon Shaa "P5" ["P5", "P5"]]
                         <&> sWall %~ ("P5" <|)

      let res = runKyokuState kyoku $ do
            drawAndTurnAction Ton $ TurnShouminkan "P5"
            stepped $ InpShout Nan $ Shout Chankan Ton "P5" ["P5"]

      requireRight res $ \case
          (_, (KyokuEnded (DealRon [win] _), _)) -> Yaku 1 "Chankan" `elem` (win^._3.vhValue.vaYaku) @? "Chankan was not yielded"
          x -> assertFailure (show x)
  ]

yakumans :: TestTree
yakumans = testGroup "Yakumans that are dependant on the whole game state"
  [ testCase "Tenhou (dealer goes out on first draw)" $ do
      kyoku <- testKyoku <&> sHands . ix Ton . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ ("P5" <|)
                         <&> pDora .~ []

      let res = runKyokuState kyoku $ do
            stepped_ InpAuto -- start
            stepped_ InpAuto -- draw
            stepped $ InpTurnAction Ton $ TurnTsumo

      requireRight res $ \case
          (_, (KyokuEnded (DealTsumo [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 13 "Tenhou"]
          x -> assertFailure (show x)

  , testCase "Chiihou (non-dealer goes out on first draw uninterrupted)" $ do
      kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ handThatWinsWithP5
                         <&> sWall %~ ("P1" <|) . ("P5" <|)
                         <&> pDora .~ []

      let res = runKyokuState kyoku $ do
            stepped_ InpAuto -- start
            stepped_ InpAuto -- draw Ton
            stepped_ InpAuto -- discard Ton
            stepped_ InpAuto -- ignore shout wait
            stepped_ InpAuto -- check end conditions
            stepped_ InpAuto -- draw Nan
            stepped $ InpTurnAction Nan $ TurnTsumo

      requireRight res $ \case
          (_, (KyokuEnded (DealTsumo [win] _), _)) -> win^._3.vhValue.vaYaku @?= [Yaku 13 "Chiihou"]
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
                           <&> pDora .~ []
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
                           <&> pDora .~ []
                           <&> sWall %~ ("P5" <|)
                           <&> pHonba .~ 2

        let res = runKyokuState kyoku $ do
                stepped_ InpAuto -- start
                stepped_ InpAuto -- draw
                stepped_ InpAuto -- discard
                stepped_ $ InpShout Nan $ Shout Ron Ton "P5" ["P4", "P3"]
                autoEndTurn

        requireRight res $ \(_evs, (KyokuEnded r, _k)) -> do
            headEx (dWinners r) ^. _2 @?= 1000 + 2 * 300
            dPayers r ^..each._2 @?= [-1000 - 2 * 300]

    , testCase "Double-ron" $ do
        kyoku <- testKyoku <&> sHands . ix Nan . handConcealed._Wrapped .~ handThatWinsWithP5Pinfu
                           <&> sHands . ix Shaa . handConcealed._Wrapped .~ handThatWinsWithP5Pinfu
                           <&> pDora .~ []
                           <&> sWall %~ (["P5"] ++)

        let res = runKyokuState kyoku $ do
                autoAndDiscard Ton $ Discard "P5" Nothing False
                stepped_ $ InpShout Nan $ Shout Ron Ton "P5" ["P4", "P3"]
                stepped_ $ InpShout Shaa $ Shout Ron Ton "P5" ["P4", "P3"]
                autoEndTurn

        requireRight res $ \(_evs, (KyokuEnded r, _k)) -> do
            map (view _2) (dWinners r) @?= [1000, 1000]
            map (view _2) (dPayers r)  @?= [-2000]
    ]

-- agari to pair
handThatWinsWithP5 :: [Tile]
handThatWinsWithP5      = ["M1", "M1", "M1", "P1", "P2", "P3", "P5", "P7", "P8", "P9", "S2", "S3", "S4"]

-- also P2
handThatWinsWithP5Pinfu :: [Tile]
handThatWinsWithP5Pinfu = ["M1", "M2", "M3", "P3", "P4", "P5", "P5", "P7", "P8", "P9", "S2", "S3", "S4"]


-- * Game flow testing combinators

autoAndDiscard k d = drawAndTurnAction k (TurnTileDiscard d)

drawAndTurnAction k action = go where
  go = do st <- gets fst
          case st of
            WaitingDiscard{} -> stepped_ (InpTurnAction k action)
            _                -> steppedSt InpAuto >> go

autoEndTurn = gets fst >>= \case
    CheckEndConditionsAfterDiscard -> stepped_ InpAuto
    KyokuEnded{}                   -> return ()
    _                              -> stepped_ InpAuto >> autoEndTurn


-- * Mechanics testing framework

-- | Basic kyoku state for testing. Some fields are hard-coded for
-- reproducible tests.
testKyoku = newKyoku fourPlayers (map tshow [1..4]) <&> pDora .~ ["M1"]
                                                    <&> sWanpai.wUraDora .~ ["M1", "M2", "M3", "M4", "M5"]

runKyokuState k ma = runStateT ma (NotBegun 0, k)

requireRight (Left err) go = assertFailure (unpack err)
requireRight (Right res) go = go res

steppedSt step = stepped_ step >> fmap fst get

stepped s = do (m, k) <- get
               (m, k, xs) <- lift $ runKyoku k (step m s)
               put (m, k)
               return xs
stepped_ s = stepped s >> return ()
