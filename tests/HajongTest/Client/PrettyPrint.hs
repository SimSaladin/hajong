------------------------------------------------------------------------------
-- | 
-- Module         : HajongTest.Client.PrettyPrint
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module HajongTest.Client.PrettyPrint (tests) where

import Hajong.Game
import Hajong.Client.PrettyPrint

tests :: TestTree
tests = testGroup "CLI PrettyPrint"
    [ testCase "Show and read Hand" $
        "S1 S2 S3 S4 S5 S6 S7 S8 S9 M1 M2 M3 G! G!" `preadAssert`
        ( map (flip Sou False) [Ii .. Chuu]
        <> map (flip Man False) [Ii .. San]
        <> [Sangen Hatsu, Sangen Hatsu])

    , testCase "Show and read Hand" $
        "M3-M3-M3" `preadAssert` Koutsu [Man San False, Man San False, Man San False] Nothing

    , testCase "PosMine Discards"  $ PosMine   souTiles `pshowAssert` "S1 S2 S3 S4 S5 S6\nS7 S8 S9         \n                 "
    , testCase "PosLeft Discards"  $ PosLeft   souTiles `pshowAssert` "   S7 S1\n   S8 S2\n   S9 S3\n      S4\n      S5\n      S6"
    , testCase "PosRight Discards" $ PosRight  souTiles `pshowAssert` "S6      \nS5      \nS4      \nS3 S9   \nS2 S8   \nS1 S7   "
    , testCase "PosFront Discards" $ PosFront  souTiles `pshowAssert` "                 \n         S9 S8 S7\nS6 S5 S4 S3 S2 S1"

    , testGroup "Filled game contains characters" $
        let game = newEmptyGS "test game"
                & addClient ("Dummy player" :: Text)
                & set gameRound (Just dummyFullState) . fromJust
            Just playerState = getPlayerState game (Player Ton)
            in pshow playerState `hunitAllInfixOf` ["|", "_", "(", ")", "(25000)" ]

    , testProperty "pread . pshow === id (Hand)"    (preadPshowEquals :: Hand -> Bool)
    , testProperty "pread . pshow === id (Mentsu)"  (preadPshowEquals :: Mentsu -> Bool)
    , testProperty "length (pshow Tile) == 2"       ((== 2) . length . pshow :: Tile -> Bool)
    , testProperty "Pretty discards (own)"   $ liftA2 propAllInfixOf (pshow . PosMine)  (map pshow :: Discards -> [Text])
    , testProperty "Pretty discards (left)"  $ liftA2 propAllInfixOf (pshow . PosLeft)  (map pshow :: Discards -> [Text])
    , testProperty "Pretty discards (right)" $ liftA2 propAllInfixOf (pshow . PosRight) (map pshow :: Discards -> [Text])
    , testProperty "Pretty discards (front)" $ liftA2 propAllInfixOf (pshow . PosFront) (map pshow :: Discards -> [Text])
    ]

-- | A state used to test the pretty printer
dummyFullState :: RiichiState
dummyFullState = RiichiState secret public
    where
        secret                      = RiichiSecret
            { _riichiWall           = riichiTiles
            , _riichiWanpai         = take 14 riichiTiles
            , _riichiHands          = mapFromList $ zip defaultPlayers (repeat fullHand)
            , _riichiWaitShoutsFrom = []
            }

        public                     = RiichiPublic
            { _riichiDora          = take 5 $ drop 60 riichiTiles
            , _riichiWallTilesLeft = 10
            , _riichiRound         = Pei
            , _riichiDealer        = Player Ton
            , _riichiTurn          = Player Ton
            , _riichiPoints        = mapFromList $ zip defaultPlayers (repeat 25000)
            , _riichiEvents        = []
            }
        
        fullHand = initHand (take 13 riichiTiles)
            & set (handPublic.handDiscards) souTiles
            . set (handPublic.handOpen) [mentsu, mentsu, mentsu, mentsu]
 
        mentsu = Koutsu (replicate 3 $ pread "M3") Nothing

souTiles :: [(Tile, Maybe Player)]
souTiles = map (flip (,) Nothing . flip Sou False) [Ii .. Chuu]

-- | pread . show == id
preadPshowEquals :: (Eq x, PrettyPrint x, PrettyRead x) => x -> Bool
preadPshowEquals = liftA2 (==) id (pread . pshow)

-- | Assert that pread input == expected and input == pshow expected
preadAssert :: (Show x, Eq x, PrettyRead x, PrettyPrint x) => Text -> x -> Assertion
preadAssert input expected =
    let calculated = pread input
    in (calculated == expected && input == pshow expected) @? (unpack . unlines)
        [ "Input:         " <> input
        , "Expected:      " <> tshow expected
        , "Input read:    " <> tshow calculated
        , "Expected read: " <> pshow expected
        ]

pshowAssert :: (Show x, PrettyPrint x) => x -> Text -> Assertion
pshowAssert x expected = pshow x == expected @? (unpack . unlines)
    [ "= Read ="    , cons '"' . flip snoc '"' $ pshow x
    , "= Expected =", cons '"' . flip snoc '"' $ expected
    , "Value: " <> tshow x
    ]
