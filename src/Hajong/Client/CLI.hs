{-# LANGUAGE TupleSections #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Client.CLI
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Hajong.Client.CLI where

import qualified System.IO
import           ClassyPrelude hiding (finally, handle, toLower)
import           Control.Concurrent.Async (race)
import           Control.Monad.Cont (runContT, ContT, callCC)
import           Control.Monad.Reader (runReaderT, ReaderT)
import           Control.Lens
import           Data.List (elemIndex)
import           Data.Char (isUpper, toLower)
import qualified Data.Text as T
import           Data.Text.IO (putStr)
import           System.Random
-- import           System.Console.Haskeline rm pkg! TODO
import qualified Network.WebSockets as WS

---------------------------------------------------------------------
import           Hajong.Game
import           Hajong.Client.PrettyPrint
import           Hajong.Connections

type ClientM = ContT () (ReaderT ClientState IO)

discardKeys :: String
discardKeys = "aoeuidhtns-mwvz"

data ClientState = ClientState
                 { _clientConn         :: Client
                 , _clientOutput       :: Text -> IO ()
                 , _clientGetChar      :: IO Char
                 , _clientGetLine      :: IO Text
                 , _clientLounge       :: MVar Lounge
                 , _clientGame         :: MVar (Maybe (GamePlayer Nick)) -- ^ The game state
                 , _clientWaiting      :: MVar Int -- ^ Wait for game n to begin
                 }

makeLenses ''ClientState

isMyNick :: Nick -> ClientM Bool
isMyNick nick = liftM (\c -> getNick c == nick) $ view clientConn

-- * Configure

newClientState :: IO (Client -> (Text -> IO ()) -> ClientState)
newClientState = (\gc gl v1 v2 v3 c o -> ClientState c o gc gl v1 v2 v3)
    <$> pure System.IO.getChar
    <*> pure consoleGetLine
    <*> newMVar (Lounge mempty mempty)
    <*> newMVar Nothing
    <*> newMVar (-1)

randomNick :: IO Nick
randomNick = liftM pack $ replicateM 5 $ randomRIO ('a', 'z')

-- * Emit and print

-- | Send an event to server.
emit :: Event -> ClientM ()
emit ev = view clientConn >>= (`unicast` ev)

-- | Output some text to user.
outNoLn :: Text -> ClientM ()
outNoLn x = view clientOutput >>= liftIO . ($ x)

out :: Text -> ClientM ()
out x = outNoLn $ x <> "\n"

-- | "withParam msg f" asks a line of input with label "msg" and calls
-- f with the input.
withParam :: Text -> (Text -> ClientM ()) -> ClientM ()
withParam prompt cc =
        outNoLn prompt >> view clientGetLine >>= liftIO >>= cc

consoleGetLine :: IO Text
consoleGetLine = do
    System.IO.hSetEcho stdin True
    l <- getLine
    System.IO.hSetEcho stdin False
    return l

-- * App

clientMain :: IO ()
clientMain = do
    cs <- newClientState
    nick <- randomNick
    System.IO.hSetBuffering stdin System.IO.NoBuffering
    System.IO.hSetBuffering stdout System.IO.NoBuffering
    System.IO.hSetEcho stdin False
    runWSClient $ \c -> cs (websocketClient nick c) putStr

runWSClient :: (WS.Connection -> ClientState) -> IO ()
runWSClient toCS = WS.runClient "localhost" 9160 "/" $ \conn -> do
    let cs    = toCS conn
        logic = do
            out "Connected to server. Type ? for help."
            emit (JoinServer $ getNick $ _clientConn cs) >> clientLogic
    runReaderT (runContT logic (\_ -> return ())) cs
    WS.sendClose conn (PartServer "Bye")

-- * Client logic

clientLogic :: ClientM ()
clientLogic = do
    getChar <- view clientGetChar
    callCC $ \k -> view clientConn
        >>= liftIO . race getChar . receive
        >>= either (inputHandler k) eventHandler
        >> clientLogic

-- | Character in: execute a command based on a character.
inputHandler :: (() -> ClientM ()) -> Char -> ClientM ()
inputHandler exit ch = do
    in_g <- isJust <$> rview clientGame
    case ch of
        'q'  -> exit ()
        '?'  -> printHelp
        '!'  -> printStatus
        '\n' -> when in_g printGameState >> printShortStatus
        ' '  -> chatMessage
        _    -> if in_g then gameInputHandler ch else loungeInputHandler ch

loungeInputHandler :: Char -> ClientM ()
loungeInputHandler ch = case ch of
    'n' -> printUsers
    'g' -> printGamesList
    'c' -> createGame
    'j' -> joinGame
    'r' -> rawCommand
    _   -> unknownCommand ch

gameInputHandler :: Char -> ClientM ()
gameInputHandler ch =
    case ch of
        'p' -> emit $ GameShout Pon
        'c' -> emit $ GameShout $ Chi (undefined ,undefined) -- TODO get the tiles
        'k' -> emit $ GameShout Kan
        'r' -> emit $ GameShout Ron
        'l' -> view clientGetChar >>= liftIO >>= loungeInputHandler
        _  | Just dt <- elemIndex (toLower ch) discardKeys
           , riichi  <- isUpper ch   -> discardTile dt riichi
           | otherwise              -> unknownCommand ch

unknownCommand :: Char -> ClientM ()
unknownCommand ch = out $ "Command `" <> pack [ch] <> "` not recognized"

eventHandler :: Event -> ClientM ()
eventHandler ev = case ev of
    JoinServer nick   -> nickJoined nick
    PartServer nick   -> nickParted nick
    LoungeInfo lounge -> loungeInfoChanged lounge
    NewGame info      -> gameCreated info
    RoundStarts gp    -> roundStarts gp
    JoinGame n nick   -> joinedGame n nick 
    GameEvents ev     -> mapM_ receivedRoundEvent ev
    GameShout shout   -> return () -- never received from server
    GameHandChanged h -> receivedNewHand h
    Message sayer msg -> out $ "<" <> sayer <> "> " <> msg
    Invalid err       -> out $ "[error] " <> err
    _ -> out $ "Received invalid event, this should NOT happen (" <> tshow ev <> ")"

-- * Events

nickJoined :: Text -> ClientM ()
nickJoined nick = do
    me <- isMyNick nick
    unless me $ do
        rmodify clientLounge $ return . over loungeNicksIdle (insertSet nick)
        out $ nick <> " has joined."

nickParted :: Text -> ClientM ()
nickParted nick = callCC $ \k -> do
    me <- isMyNick nick
    when me $ out "You have left the server" >> k ()
    rmodify clientLounge $ return
        . (loungeNicksIdle %~ deleteSet nick)
        . ((loungeGames.each._2) %~ deleteSet nick)
    out $ nick <> " has parted."

gameCreated :: (Int, Text, Set Nick) -> ClientM ()
gameCreated (n, name, nicks) = do
    out $ "New game created: `" <> ppGame n (name, nicks)
    rmodify clientLounge $ return . over loungeGames (insertMap n (name, nicks))
 
loungeInfoChanged :: Lounge -> ClientM ()
loungeInfoChanged lounge = rswap clientLounge lounge >> printLounge

joinedGame :: Int -> Text -> ClientM ()
joinedGame n nick = do
    rmodify clientLounge $ return
        . over (loungeGames.at n.traversed._2) (insertSet nick)
        . over loungeNicksIdle (deleteSet nick)

    lounge <- rview clientLounge
    let count = length $ view (loungeGames.at n.traversed._2) lounge
        countInfo
            | count < 4 = tshow (4 - count) <> " more players until game starts." 
            | otherwise = " Game is starting!"

    me <- isMyNick nick
    if me
        then do _ <- rswap clientWaiting n
                out $ "Joined the game (" <> tshow n <> "). " <> countInfo
        else out $ nick <> " joined game (" <> tshow n <> "). " <> countInfo

roundStarts :: GamePlayer Nick -> ClientM ()
roundStarts gp = do
    out "The round begins now!"
    _ <- rswap clientGame (Just gp)
    printGameState

receivedRoundEvent :: RoundEvent -> ClientM ()
receivedRoundEvent ev = case ev of
    RoundTurnAction p ta -> receivedTurnAction p ta

    RoundPublicHand p hp ->
        out "A public hand changed TODO"

    RoundTsumo p         ->
        out "Round was won by tsumo"
        -- TODO

    RoundRon p fps       ->
        out "Round was won by a ron"
        -- TODO

    RoundDraw tps        ->
        out "Round ended in an exhaustive draw"
        -- TODO

receivedTurnAction :: Player -> TurnAction -> ClientM ()
receivedTurnAction p ta = case ta of
    TurnTileDiscard riichi tile -> receivedDiscard riichi tile
    TurnTileDraw dead mt        -> receivedTileDraw dead mt
    TurnAnkan tile              -> receivedAnkan tile
    TurnShouted shout who       -> receivedShout shout who
    TurnAuto                    -> return ()

receivedDiscard :: Bool -> Tile -> ClientM ()
receivedDiscard riichi tile = 
    out $ "Discarded " <> pshow tile <> "." <> if riichi then " Riichi!" else ""
    -- TODO change state?

receivedTileDraw :: Bool -> Maybe Tile -> ClientM ()
receivedTileDraw dead mt = 
    out $ "Draw " <> maybe "" ((<> " ") . pshow) mt <> if dead then "from wanpai." else ""
    -- TODO change state?

receivedShout :: Shout -> Player -> ClientM ()
receivedShout shout player = 
    out $ pshow player <> ": " <> pshow shout
    -- TODO Change state?

receivedNewHand :: Hand -> ClientM ()
receivedNewHand h = rmodify clientGame $ return . set (_Just.playerMyHand) h
    -- TODO something happens here?

receivedAnkan :: Tile -> ClientM ()
receivedAnkan t = out $ "Called " <> pshow t <> " ankan."
    -- TODO change state?

-- * Actions

createGame :: ClientM ()
createGame = withParam "Game name: " $ \name ->
        let name' = T.dropWhileEnd (== ' ') $ T.dropWhile (== ' ') name
            in case name' of
                 "" -> out "Name cannot be empty"
                 _  -> emit $ CreateGame name'

joinGame :: ClientM ()
joinGame = printGamesList >> withParam "Join game: " go
    where
        go str = case readMay str of
            Nothing -> out $ "NaN: " <> str
            Just n  -> emit $ JoinGame n ""

-- ** Shout

-- ** Discard

discardOptions :: ClientM [(Tile, Bool)] -- ^ (discard, can riichi?)
discardOptions = do
    game <- rview clientGame <&> (^?! _Just)
    let tiles = game^.playerMyHand.handConcealed ++ maybeToList (game^.playerMyHand.handPick)
    return $ map (,True) tiles -- TODO check if can riichi

-- ^ "discardTile nth with_riichi"
discardTile :: Int -> Bool -> ClientM ()
discardTile n riichi = do
    tiles <- discardOptions
    case tiles ^? traversed.index n of
        Nothing             -> out $ "No tile for " <> tshow n
        Just (tile, can_riichi)
            | False <- can_riichi
            , True <- riichi -> out "Cannot riichi with that tile"
            | otherwise     -> emit $ GameAction $ TurnTileDiscard riichi tile

-- ** Other

chatMessage :: ClientM ()
chatMessage = withParam "say: " send
    where
        send ""  = return ()
        send msg = emit $ Message "" msg

rawCommand :: ClientM ()
rawCommand = withParam "send command: " $
        maybe (out "Command not recognized") emit . readMay

-- ** Print

printShortStatus :: ClientM ()
printShortStatus = do
    i_am   <- getNick <$> view clientConn
    inGame <- rview clientGame
    game_n <- rview clientWaiting
    let status = if game_n >= 0
                     then maybe "wait" (const "game") inGame <> " " <> tshow game_n
                     else "idle"
    out $ "(" <> status <> ") <" <> i_am <> "> "

printHelp :: ClientM ()
printHelp = out $ unlines
    [ "Every command is initiated by it's first [l]etter"
    , ""
    , "Global commands"
    , "  [?] (help)                 Show this help text"
    , "  [!] (status)               Show status information"
    , "  <Space> (message)          Open a line prompt, send with enter."
    , "  [q]uit                     Close the client"
    , ""
    , "Messages are received in lounge or in-game only, corresponding to"
    , "your location. Use `l ` to send messages to lounge from a game"
    , ""
    , "In lounge"
    , "  [n]ames                    Show idle users"
    , "  [g]ames                    Show all games and players"
    , "  [c]reate <name>            Create a new game"
    , "  [j]oin <name>              Join game"
    , "  [r]aw <cmd>                Send direct protocol command (for debugging only)"
    , ""
    , "In game"
    , "  [aoeuidhtns-mwvz]          Discard [n]:th tile"
    , "  [p]on [c]hi [k]an [r]on    Shout a discard or declare kantsu"
    , "  [l]ounge                   Interpret next letter as a lounge commend"
    ]

printStatus :: ClientM ()
printStatus = do
    mgame       <- rview clientGame
    gameWait    <- rview clientWaiting
    case mgame of
        Nothing -> out $ "In lounge" <> if gameWait >= 0 then ", ready for game n. " <> tshow gameWait else ""
        Just _  -> out $ "In game n." <> tshow gameWait

printUsers :: ClientM ()
printUsers = do
    lounge <- view clientLounge >>= liftIO . readMVar
    out $ "Users idle: " <> ppNicks (lounge ^. loungeNicksIdle)

printGamesList :: ClientM ()
printGamesList = do
    lounge <- rview clientLounge
    case lounge^.loungeGames & imap ppGame & toListOf each of
       []     -> out "No games"
       (x:xs) -> out $ unlines $ "Games: " <> x : map ("       " <>) xs

printLounge :: ClientM ()
printLounge = printUsers >> printGamesList

printGameState :: ClientM ()
printGameState = 
    rview clientGame >>= out . maybe "Round is not active!" pshow
