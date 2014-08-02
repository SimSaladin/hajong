------------------------------------------------------------------------------
-- | 
-- Module         : GameMechanics
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GameMechanics (mechanicsTests) where

import Hajong.Game

mechanicsTests :: TestTree
mechanicsTests = testGroup "Game mechanics"
  [ newRiichiStateCheck
  ]

newRiichiStateCheck :: TestTree
newRiichiStateCheck = testCase "Check properties of `newRiichiState`" $ do
    RiichiState{ _riichiSecret = secret
               , _riichiPublic = public
               } <- newRiichiState

    secret^.riichiWall.to length                         == 136-14-4*13       @? "Wall of 136-14-4*13 tiles"
    secret^.riichiWanpai.to length                       == 13                @? "Wanpai of 13 (+1 dora) tiles"
    secret^.riichiHands ^.. each.handConcealed.to length == [13, 13, 13, 13]  @? "Four hands of 13 tiles"
    public^.riichiDora.to length                         == 1                 @? "One dora tile"
    public^.riichiRound == Ton @? "First round was not Ton"

--   , testCase "A deal can be done after 4 players join" $
--          assertBool "game was not initialized" $ isJust readyServerState
--
--   , testCase "Only the player in turn can draw" undefined
--   , testCase "Discarding a tile in riichi results in error" undefined
--   , testCase "Typical mentsu tenpai"    undefined
--   , testCase "Chiitoitsu tenpai"                            undefined
--   ]
