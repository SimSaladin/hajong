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
import Mahjong.Hand

import qualified Data.Map as M

tests :: TestTree
tests = testGroup "Game mechanics"
  [ winningTest
  ]

testKyoku = Kyoku { _pRound = Ton, _pDeal = 1, _pTurn = Ton, _pOja = Player 3, _pFirstOja = Player 3, _pWallTilesLeft = 70, _pDora = ["S3"]
                  , _pPlayers = M.fromList [(Player 1,(Ton,25000,"Player 1")),(Player 2,(Nan,25000,"Player 2")),(Player 3,(Shaa,25000,"Player 3")),(Player 4,(Pei,25000,"Player 4"))]
                  , _pHonba = 0, _pRiichi = 0, _pResults = Nothing, _pDeals = [], _sEvents = []
                  , _sHands = M.fromList [(Ton,Hand {_handConcealed = ["S1","M3","R!" ,"S6","P9","S9","R!" ,"P9","S7","M6","P6","M6","M2"], _handPick = Nothing, _handFuriten = Nothing, _handPublic = HandPublic {_handCalled = [], _handDiscards = [], _handRiichi = False, _handDrawWanpai = False, _hLastFromWanpai = False, _handAgari = Nothing, _hIppatsu = False, _handAgariCall = Nothing, _hDoubleRiichi = False, _hIsConcealed = False}, _hCanTsumo = False})
                                         ,(Nan,Hand {_handConcealed = ["M8","S7","P5","S8","P7","P7","P1","W " ,"S " ,"S1","S1","M5","S6"], _handPick = Nothing, _handFuriten = Nothing, _handPublic = HandPublic {_handCalled = [], _handDiscards = [], _handRiichi = False, _handDrawWanpai = False, _hLastFromWanpai = False, _handAgari = Nothing, _hIppatsu = False, _handAgariCall = Nothing, _hDoubleRiichi = False, _hIsConcealed = False}, _hCanTsumo = False})
                                         ,(Shaa,Hand {_handConcealed = ["E ","M3","P7","S5","S9","S8","S6","P1","G!" ,"N " ,"M6","S4","W!" ], _handPick = Nothing, _handFuriten = Nothing, _handPublic = HandPublic {_handCalled = [], _handDiscards = [], _handRiichi = False, _handDrawWanpai = False, _hLastFromWanpai = False, _handAgari = Nothing, _hIppatsu = False, _handAgariCall = Nothing, _hDoubleRiichi = False, _hIsConcealed = False}, _hCanTsumo = False})
                                         ,(Pei,Hand {_handConcealed = ["P2","P9","P5","P3","P5","P1","P1","M4","M9","M1","S9","P8","S " ], _handPick = Nothing, _handFuriten = Nothing, _handPublic = HandPublic {_handCalled = [], _handDiscards = [], _handRiichi = False, _handDrawWanpai = False, _hLastFromWanpai = False, _handAgari = Nothing, _hIppatsu = False, _handAgariCall = Nothing, _hDoubleRiichi = False, _hIsConcealed = False}, _hCanTsumo = False})]
                  , _sWall = ["M1","M4","P2","G!" ,"M8","S2","S5","P3","P9","S3","P7","S5","M3","M6","P6","S " ,"P2","P4","P6","M4","G!" ,"S2","P8","M2","S5","P8","P4","M9","S2","W!" ,"M9","M4","M1","E " ,"W!" ,"M5","P8","M1","M7","S " ,"E " ,"S7","P3","M8","S9","M7","N " ,"S4","P5","E " ,"W " ,"R!" ,"M7","S4","P6","M8","M2","G!" ,"S4","S2","W " ,"R!" ,"S8","M7","M9","P3","W!" ,"S3","S1","W " ]
                  , _sWanpai = ["S3","N " ,"M2","S8","M3","P4","S7","P2","M5","N " ,"M5","P4","S6"]
                  , _sWaiting = Nothing }

winningTest :: TestTree
winningTest = testCase "Game ends when a Ron is called" $ do
    let tk = testKyoku & sHands.ix Nan .handConcealed .~ ["M5", "M5", "M5", "P5", "P6", "P7", "P8", "P8", "S4", "S5", "S6", "S7", "S8"]
                       & sHands.ix Ton .handConcealed .~ ["S3"]

        go k f = runRoundM f $ GameState (_pPlayers k) "" (Just k)

        res = do
            (_,k,_) <- go tk startDeal
            (_,k,_) <- go k $ startTurn Ton
            (_,k,_) <- go k $ runTurn' Ton (TurnTileDraw False Nothing)
            error (show k)
            (_,k,_) <- go k $ runTurn' Ton (TurnTileDiscard (Mahjong.Discard "S3" Nothing False))
            go k $ advanceWithShout (Shout Ron Ton "S3" ["S4", "S5"]) (Player 1)

    case res of
        Left err -> assertFailure (unpack err)
        Right (Just results, _, _) -> return ()
        Right (Nothing, _, _)      -> assertFailure "Kyoku didn't end after a Ron"

--         testCase "Check properties of `newRiichiState`" $ do
--     RiichiState{ _riichiSecret = secret
--                , _riichiPublic = public
--                } <- newRiichiState
-- 
--     secret^.riichiWall.to length                         == 136-14-4*13       @? "Wall of 136-14-4*13 tiles"
--     secret^.riichiWanpai.to length                       == 13                @? "Wanpai of 13 (+1 dora) tiles"
--     secret^.riichiHands ^.. each.handConcealed.to length == [13, 13, 13, 13]  @? "Four hands of 13 tiles"
--     public^.riichiDora.to length                         == 1                 @? "One dora tile"
--     public^.riichiRound == Ton @? "First round was not Ton"

--   , testCase "A deal can be done after 4 players join" $
--          assertBool "game was not initialized" $ isJust readyServerState
--
--   , testCase "Only the player in turn can draw" undefined
--   , testCase "Discarding a tile in riichi results in error" undefined
--   , testCase "Typical mentsu tenpai"    undefined
--   , testCase "Chiitoitsu tenpai"                            undefined
--   ]
