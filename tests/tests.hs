{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

import ClassyPrelude hiding (assert)
import Control.Lens hiding (elements, snoc, cons)
import Control.Concurrent (threadDelay, forkIO)
import Control.Applicative
import Control.Monad.State  (StateT(..), evalStateT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Knob
import Data.Maybe (fromJust)
import System.IO (IOMode(..))
import System.IO.Silently
import System.IO.Temp
import System.Posix
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Property as Q

import CLI
import Tiles
import Server
import Riichi
import PrettyPrint

main :: IO ()
main = do
    putStrLn "Starting server process :9160..."
    pid <- startServer

    defaultMain tests `finally` do
        putStrLn "Killed server process."
        signalProcess killProcess pid

tests :: TestTree
tests = testGroup "Hajong tests"
    [ clientTests, prettyPrintQC, prettyPrintHUnit, gameTests ]

-- * Game

gameTests :: TestTree
gameTests = testGroup "Pure game state tests"
    [ testCase "New game initialized right" $ do
        (secret, public) <- newRiichiState

        secret^.riichiWall.to length                         == 136-14-4*13       @? "Wall of 136-14-4*13 tiles"
        secret^.riichiWanpai.to length                       == 13                @? "Wanpai of 13 (+1 dora) tiles"
        secret^.riichiHands ^.. each.handConcealed.to length == [13, 13, 13, 13]  @? "Four hands of 13 tiles"

        public^.riichiDora.to length                         == 1                 @? "One dora tile"
        public^.riichiRound == Ton @? "First round was not Ton"

    , testCase "Discarding a tile in riichi results in error" undefined
    , testCase "Typical mentsu tenpai"    undefined
    , testCase "Chiitoitsu tenpai"                            undefined

    ]

-- | PrettyPrint
prettyPrintHUnit :: TestTree
prettyPrintHUnit = testGroup "PrettyPrint HUnit"
    [ testCase "Show and read Hand" $
        "S1 S2 S3 S4 S5 S6 S7 S8 S9 M1 M2 M3 G! G!" `preadAssert`
        ( map (flip Sou False) [Ii .. Chuu]
        <> map (flip Man False) [Ii .. San]
        <> [Sangen Hatsu, Sangen Hatsu])

    , testCase "Show and read Hand" $
        "M3-M3-M3" `preadAssert` Koutsu [Man San False, Man San False, Man San False] True

    , testCase "DiscardPileOwn"   $ DiscardPileOwn   souTiles `pshowAssert`
            "S1 S2 S3 S4 S5 S6\nS7 S8 S9         \n                 "

    , testCase "DiscardPileLeft"  $ DiscardPileLeft  souTiles `pshowAssert`
            "   S7 S1\n   S8 S2\n   S9 S3\n      S4\n      S5\n      S6"

    , testCase "DiscardPileRight" $ DiscardPileRight souTiles `pshowAssert`
            "S6      \nS5      \nS4      \nS3 S9   \nS2 S8   \nS1 S7   "

    , testCase "DiscardPileFront" $ DiscardPileFront souTiles `pshowAssert`
            "                 \n         S9 S8 S7\nS6 S5 S4 S3 S2 S1"

    , testCase "Complete game"  $ do
        game <- newGameServer "test game" & gsAddPlayer ("Dummy player" :: Text) & fromJust . gsNewGame . fromJust
        let Just pstate = gsPlayerLookup game (Player 0)
        pstate `pshowAssert` ""
    ]

prettyPrintQC :: TestTree
prettyPrintQC = testGroup "PrettyPrint QC"
    [ testProperty "pread . pshow === id (Hand)"    (preadPshowEquals :: Hand -> Bool)
    , testProperty "pread . pshow === id (Mentsu)"  (preadPshowEquals :: Mentsu -> Bool)
    , testProperty "length (pshow Tile) == 2"       ((== 2) . length . pshow :: Tile -> Bool)
    , testProperty "Pretty discards (own)" $ liftA2 propAllInfixOf (pshow . DiscardPileOwn) (map pshow)
    , testProperty "Pretty discards (left)" $ liftA2 propAllInfixOf (pshow . DiscardPileLeft) (map pshow)
    , testProperty "Pretty discards (right)" $ liftA2 propAllInfixOf (pshow . DiscardPileRight) (map pshow)
    , testProperty "Pretty discards (front)" $ liftA2 propAllInfixOf (pshow . DiscardPileFront) (map pshow)
    ]

-- | pread . show == id
preadPshowEquals :: (Eq x, PrettyPrint x, PrettyRead x) => x -> Bool
preadPshowEquals = liftA2 (==) id (pread . pshow)

propAllInfixOf :: Text -> [Text] -> Property
propAllInfixOf result xs = conjoin $ flip map xs $
    \x -> if x `isInfixOf` result
              then Q.succeeded
              else Q.failed { Q.reason = unpack $ x <> " not found in result" }

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

souTiles :: [(Tile, Maybe Player)]
souTiles = map (flip (,) Nothing . flip Sou False) [Ii .. Chuu]

-- * Client

-- | Tests for client side
clientTests :: TestTree
clientTests = testGroup "Client tests"
    [ clientCLILoungeTests
    , clientCLIGameTests
    ]

clientCLILoungeTests :: TestTree
clientCLILoungeTests = testGroup "CLI Lounge Unit Tests"
    [ testCase "connect + terminate on q"           $ runClient [   ]                           =~ "Connected to server"
    , testCase "? shows the help message"           $ runClient ["?"]                           =~ "Show this help text"
    , testCase "! shows status as lounge on start"  $ runClient ["!"]                           =~ "In lounge"
    , testCase "<Space>[message] gives prompt"      $ runClient [" ","The message"]             =~ "say:"
    , testCase "n lists idle users"                 $ runClient ["n"]                           =~ "Users idle:"
    , testCase "g lists games"                      $ runClient ["g"]                           =~ "ames"
    , testCase "c gives create prompt"              $ runClient ["c","Named"]                   =~ "Game name"
    , testCase "c doesn't accept empty name"        $ runClient ["c"," "]                       =~ "empty"
    , testCase "j opens join prompt"                $ runClient ["j","-1"]                      =~ "join game"  -- TODO
    , testCase "r sends a raw command"              $ runClient ["r","Message \"from\" \"me\""] =~ "send command"
    ]

clientCLIGameTests :: TestTree
clientCLIGameTests = testGroup "CLI In-Game unit tests"
    [ testCase "joining bogus game raises error"                      $ runClient ["j","-1" ] =~ "[error]"
    , testCase "joining game works (NOTE: requires game 0 on server)" $ runClient ["j","0"]   =~ "Joined the game"
    , testCase "game starts when 4 clients have joined" $ return ()
    ]

type ClientInputDummy = StateT (MVar Text, TChan Text) (ReaderT ClientState IO)

instance ClientOutput ClientInputDummy where
    outAll xs = do
        chan <- use _2
        liftIO . atomically $ mapM_ (writeTChan chan) xs

instance ClientInput ClientInputDummy where
    askChar   prompt   = out prompt >> use _1 >>= liftIO . takeMVar & liftM (Just . unsafeHead)
    withParam prompt f = out prompt >> use _1 >>= liftIO . takeMVar >>= f

runClient :: [Text] -> IO Text
runClient xs = do
    inputVar <- newEmptyMVar
    _ <- forkIO $ mapM_ (\x -> threadDelay 500000 >> putMVar inputVar x) $ xs ++ ["q"]
    runClient' inputVar <&> unlines

runClient' :: MVar Text -> IO [Text]
runClient' inputVar = do
    outputChan <- newBroadcastTChanIO
    outputReadChan <- atomically $ dupTChan outputChan

    let runDummy ma = runReaderT $ evalStateT ma (inputVar, outputChan)
        loop        = inputLoop loop

    clientMain' (runDummy loop) (runDummy clientReceiver)

    let readChan = tryReadTChan outputReadChan >>= maybe (return []) (\x -> (x :) <$> readChan)
        in atomically readChan

-- | "action =~ expected" => expected `isInfixOf` (result of action)
(=~) :: IO Text -> Text -> IO ()
f =~ t = f >>= \res -> isInfixOf t res @? unpack (unlines ["== Got ==", res, "\n== Expected ==", t])

-- * Server

-- | Start the server process silenced
startServer :: IO ProcessID
startServer = do
    pid <- forkProcess $ hSilence [stdout, stderr] serverMain
    threadDelay 1000000
    return pid


-- * Arbitrary instancees

instance Arbitrary Hand where
    arbitrary = initHand <$> vector 13

instance Arbitrary Tile where
    arbitrary = elements riichiTiles

instance Arbitrary Mentsu where
    arbitrary = oneof
        [ arbitrary >>= \tile -> return (Kantsu (replicate 4 tile) True)
        , arbitrary >>= \tile -> return (Koutsu (replicate 3 tile) True)
        , arbitrary >>= \tile -> return (Jantou (replicate 2 tile) True)
        , do
             tile <- arbitrary `suchThat` (\tile -> tileSuited tile && tileNumber tile <= Chii)
             let n = tileNumber tile
             return $ Shuntsu (tile : map (setTileNumber tile) [succ n, succ (succ n)]) True
        ]


instance Arbitrary Player where
    arbitrary = elements defaultPlayers
