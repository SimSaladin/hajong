------------------------------------------------------------------------------
-- | 
-- Module         : MahjongTest.Mechanics
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module MahjongTest.Mechanics (tests) where

import Mahjong
import Mahjong.Hand as Hand

import qualified Data.Map as M

tests :: TestTree
tests = testGroup "Game mechanics"
  [ winningTest
  , discardTest
  ]

winningTest :: TestTree
winningTest = testCase "Game ends when a Ron is called" $ do
    kyoku <- newKyoku (map Player [1..4]) (map tshow [1..4])
        <&> sHands.ix Nan .handConcealed . _Wrapped .~ ["M5", "M5", "M5", "P5", "P6", "P7", "P8", "P8", "S4", "S5", "S6", "S7", "S8"]
        <&> sHands.ix Ton .handConcealed . _Wrapped .~ ["S3"]

    let res = do
            (m,k,_) <- runKyoku kyoku (step NotBegun InpAuto)
            (m,k,_) <- runKyoku k $ step m $ InpTurnAction Ton $ TurnTileDraw False Nothing
            (m,k,_) <- runKyoku k $ step m $ InpTurnAction Ton $ TurnTileDiscard $ Mahjong.Discard "S3" Nothing False
            runKyoku k $ step m $ InpShout Nan $ Shout Ron Ton "S3" ["S4", "S5"]

    case res of
        Left err                    -> assertFailure (unpack err)
        Right (Ended results, _, _) -> return ()
        Right (m, _, _)             -> assertFailure $ "Expected 'Ended' but received " <> show m

discardTest :: TestTree
discardTest = testCase "Discarding a tile results in correct state and yields correct GameEvents" $ do
    kyoku <- newKyoku fourPlayers (map tshow [1..4])

    let Just tile = kyoku ^? sHands . ix Ton . handConcealed . _Wrapped . _head

        res = do
            (m,k,_) <- runKyoku kyoku (step NotBegun InpAuto)
            (m,k,_) <- runKyoku k $ step m $ InpTurnAction Ton $ TurnTileDraw False Nothing
            runKyoku k $ step m $ InpTurnAction Ton $ TurnTileDiscard $ Mahjong.Discard tile Nothing False

    case res of
        Left err -> assertFailure (unpack err)
        Right (_, k, xs) -> k ^?! sHands . ix Ton . handDiscards == [Hand.Discard tile Nothing False] @? "Discard didn't equal"


stateInitTest :: TestTree
stateInitTest = testCase "New kyoku state has correct properties" $ do
    kyoku <- newKyoku fourPlayers (map tshow [1..4])

    kyoku^.sWall.to length                         == 136-14-4*13       @? "Wall of 136-14-4*13 tiles"
    kyoku^.sWanpai.to length                       == 13                @? "Wanpai of 13 (+1 dora) tiles"
    kyoku^.sHands ^.. each.handConcealed._Wrapped.to length == [13, 13, 13, 13]  @? "Four hands of 13 tiles"
    kyoku^.pDora.to length                         == 1                 @? "One dora tile"
    kyoku^.pRound == Ton @? "First round was not Ton"

--   , testCase "Only the player in turn can draw" undefined
--   , testCase "Discarding a tile in riichi results in error" undefined
--   , testCase "Typical mentsu tenpai"    undefined
--   , testCase "Chiitoitsu tenpai"                            undefined
