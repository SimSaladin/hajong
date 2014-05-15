{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import Prelude ((!!))
import ClassyPrelude hiding (assert)
import Control.Lens hiding (elements)
import Control.Concurrent hiding (newMVar, withMVar, modifyMVar_)
import Control.Applicative
import Data.Knob
import Data.Maybe
import System.IO (IOMode(..))
import System.IO.Silently
import System.IO.Temp
import System.IO.Unsafe
import System.Posix
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

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
    [ clientCLILoungeTests
    , clientCLIGameTests
    , gameTests
    , prettyPrintTests
    ]

clientCLILoungeTests :: TestTree
clientCLILoungeTests = testGroup "CLI Lounge Unit Tests"
    [ testCase "connect + terminate on q"           $ runClient "" =~ "Connected to server"
    , testCase "? shows the help message"           $ runClient "?" =~ "Show this help text"
    , testCase "! shows status as lounge on start"  $ runClient "!" =~ "In lounge"
    , testCase "<Space>[message] gives prompt"      $ runClient " The message\n" =~ "say:"
    , testCase "n lists idle users"                 $ runClient "n" =~ "Users idle:"
    , testCase "g lists games"                      $ runClient "g" =~ "Games"
    , testCase "c gives create prompt"              $ runClient "cNamed\n" =~ "Game name"
    , testCase "c doesn't accept empty name"        $ runClient "c \n" =~ "empty"
    , testCase "j opens join prompt"                $ runClient "j-1\n" =~ "join game"  -- TODO
    , testCase "r sends a raw command"              $ runClient "rMessage \"from\" \"me\"\n" =~ "send command"
    ]

clientCLIGameTests :: TestTree
clientCLIGameTests = testGroup "CLI In-Game unit tests"
    [ testCase "joining bogus game raises error"                      $ runClient "j-1\n" =~ "[error]"
    , testCase "joining game works (NOTE: requires game 0 on server)" $ runClient "j0\n"  =~ "Ready to start game 0"
    , testCase "game starts when 4 clients have joined" $ return ()
    ]

-- | Tests on pure game
gameTests :: TestTree
gameTests = testGroup "Game tests"
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
prettyPrintTests :: TestTree
prettyPrintTests = testGroup "QC PrettyPrint tests"
    [ testGroup "PrettyPrint QC"

        [ testProperty "pread . pshow === id (Hand)" (preadPshowEquals :: Hand -> Bool)
        , testProperty "pread . pshow === id (Mentsu)" (preadPshowEquals :: Mentsu -> Bool)
        ]

    , testGroup "PrettyPrint HUnit"

        [ testCase "Hand PrettyRead and -Print" $ preadAssert "S1 S2 S3 S4 S5 S6 S7 S8 S9 M1 M2 M3 G! G!"
            $ map (flip Sou False) [Ii .. Chuu] <> map (flip Man False) [Ii .. San] <> [Sangen Hatsu, Sangen Hatsu]

        , testCase "Mentsu PrettyRead and -Print" $ preadAssert "M3-M3-M3"
            (Koutsu [Man San True, Man San True, Man San True] True)
        ]
    ]

-- * Game

-- | pread . show == id
preadPshowEquals :: (Eq x, PrettyPrint x, PrettyRead x) => x -> Bool
preadPshowEquals = liftA2 (==) id (pread . pshow)

-- | Assert that pread input == expected
preadAssert :: (Eq x, PrettyRead x) => Text -> x -> Assertion
preadAssert input expected = pread input == expected @? "Not as expected: " <> unpack input

randomHand :: Hand
randomHand = unsafePerformIO newRiichiState & fromJust . view (_1.riichiHands.at 0)

-- * Client

(=~) :: IO Text -> Text -> IO ()
f =~ t = f >>= \res -> isInfixOf t res @? unpack (unlines ["== Got ==", res, "\n== Expected ==", t])

-- | Run a client with predefined input
runClient :: ByteString -> IO Text
runClient input = do
    putStrLn "runClient"
    (fpRes, hRes) <- openTempFile "/tmp" "hajong-test-res.log"
    hClose (fpRes `seq` hRes)

    pid <- forkProcess $ clientProcess fpRes input
    _ <- getProcessStatus True False pid

    finally (readFile $ fpFromString fpRes) (removeLink fpRes)

clientProcess :: String -> ByteString -> IO ()
clientProcess fp input = do
    k <- newKnob (input <> "q")
    (theOutput, res) <-
        withFileHandle k "knob" ReadMode $
        capture . timeout 3000000 . clientMain . Just
    writeFile (fpFromString fp) (theOutput <> if isNothing res then "Client didn't terminate" else "")

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

