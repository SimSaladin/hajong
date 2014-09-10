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

import           Control.Concurrent.Async ( race )
import           Control.Monad.Cont       ( runContT, ContT, callCC )
import           Control.Monad.Reader     ( runReaderT, ReaderT )
import           Data.List                ( elemIndex )
import qualified Data.Char                ( isUpper, toLower )
import qualified Data.Text          as T
import           Data.Text.IO             ( putStr )
import           System.Random
import qualified Network.WebSockets as WS
import qualified System.IO

---------------------------------------------------------------------
import           Mahjong
import           Hajong.Client.PrettyPrint
import           Hajong.Connections

type ClientM = ContT () (ReaderT ClientState IO)

data ClientState = ClientState
                 { _clientConn         :: Client
                 , _clientOutput       :: Text -> IO ()
                 , _clientGetChar      :: IO Char
                 , _clientGetLine      :: IO Text
                 , _clientLounge       :: TVar Lounge
                 , _clientGame         :: TVar (Maybe GamePlayer) -- ^ The game state
                 , _clientWaiting      :: TVar Int -- ^ Wait for game n to begin
                 }

makeLenses ''ClientState

newClientState :: IO (Client -> (Text -> IO ()) -> ClientState)
newClientState = (\gc gl v1 v2 v3 c o -> ClientState c o gc gl v1 v2 v3)
    <$> pure System.IO.getChar
    <*> pure consoleGetLine
    <*> newTVarIO (Lounge mempty mempty)
    <*> newTVarIO Nothing
    <*> newTVarIO (-1)

randomNick :: IO Nick
randomNick = liftM pack $ replicateM 5 $ randomRIO ('a', 'z')

discardKeys :: String
discardKeys = "aoeuidhtns-mwvz"

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

-- * Emit, output and read

class Emittable e where
    toEvent :: e -> Event
instance Emittable Event where toEvent = id
instance Emittable TurnAction where toEvent = InGameAction . GameTurn
instance Emittable Shout where toEvent = InGameAction . GameShout
instance Emittable () where toEvent _ = InGameAction GameDontCare

-- | Send an event to server.
emit :: Emittable e => e -> ClientM ()
emit ev = view clientConn >>= (`unicast` toEvent ev)

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

-- * Client logic

clientLogic :: ClientM ()
clientLogic = do
    getChar <- view clientGetChar
    callCC $ \k -> view clientConn
        >>= liftIO . race getChar . receive
        >>= either (inputHandler k) eventHandler
        >> clientLogic

-- ** Input handlers

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
    _   -> unknownCommand ch

gameInputHandler :: Char -> ClientM ()
gameInputHandler ch
    | 'l' <- ch  = oneLoungeCmd
    | Just my_dt <- elemIndex (Data.Char.toLower ch) discardKeys
                = discardTile my_dt (Data.Char.isUpper ch )
    | otherwise = lastDiscard >>= maybe noLastDiscard handleShouts
    where
        oneLoungeCmd  = view clientGetChar >>= liftIO >>= loungeInputHandler
        noLastDiscard = unknownCommand ch
        handleShouts (tp, (dt,_)) = case ch of
            'p' -> emit $ Pon tp dt
            'c' -> emit $ Chi tp dt [] -- TODO get the tiles
            'k' -> emit $ Kan tp dt
            'r' -> emit $ Ron tp dt [] -- TODO 
            _   -> unknownCommand ch

unknownCommand :: Char -> ClientM ()
unknownCommand ch = out $ "Command `" <> pack [ch] <> "` not recognized"

-- * Event handlers

eventHandler :: Event -> ClientM ()
eventHandler ev = case ev of
    InGamePrivateEvent ge -> gameEventHandler ge
    InGameEvents evs      -> mapM_ gameEventHandler evs
    Message sayer msg     -> out $ "<" <> sayer <> "> " <> msg
    Invalid err           -> out $ "[error] " <> err
    LoungeInfo lounge     -> rswap clientLounge lounge >> printLounge

    JoinServer nick -> do
            me <- isMyNick nick
            unless me $ do
                rmodify clientLounge $ loungeNicksIdle %~ insertSet nick
                out $ nick <> " has joined."

    PartServer nick -> callCC $ \k -> do
            me <- isMyNick nick
            when me $ out "You have left the server" >> k ()
            rmodify clientLounge
                $ (loungeNicksIdle %~ deleteSet nick)
                . ((loungeGames.each._2) %~ deleteSet nick)
            out $ nick <> " has parted."

    GameCreated (n, name, nicks) -> do
            out $ "New game created: `" <> ppGame n (name, nicks)
            rmodify clientLounge $ loungeGames %~ insertMap n (name, nicks)

    JoinGame n nick -> do
            rmodify clientLounge
                $ over (loungeGames.at n.traversed._2) (insertSet nick)
                . over loungeNicksIdle (deleteSet nick)

            lounge <- rview clientLounge
            let players = lounge^.loungeGames.at n.traversed._2
                countInfo
                    | length players < 4 = tshow (4 - length players) <> " more players until game starts." 
                    | otherwise          = " Game is starting!"
            me <- isMyNick nick
            if me
                then do _ <- rswap clientWaiting n
                        out $ "Joined the game (" <> tshow n <> "). " <> countInfo
                else out $ nick <> " joined game (" <> tshow n <> "). " <> countInfo

    _ -> out $ "Received invalid event, this should NOT happen (" <> tshow ev <> ")"

gameEventHandler :: GameEvent -> ClientM ()
gameEventHandler ev = do
    rmodify clientGame $ _Just %~ applyGameEvent ev
    case ev of
        RoundPrivateStarts gp -> do
                out "The round begins now!"
                _ <- rswap clientGame (Just gp)
                printGameState

        RoundPrivateChange _p _h -> out "My hand changed"
        RoundTurnBegins p        -> beginTurn p
        RoundTurnAction p ta     -> turnActionHandler p ta
        RoundTurnShouted p shout -> out $ pshow p <> " shouts " <> pshow shout
        RoundHandChanged p hp    -> out "Someones hand changed"
        RoundEnded res           -> case res of
            RoundTsumo{} -> out "Round was won by tsumo"
            RoundRon{}   -> out "Round was won by a ron"
            RoundDraw{}  -> out "Round ended in an exhaustive draw"

turnActionHandler :: Player -> TurnAction -> ClientM ()
turnActionHandler _p ta = case ta of
    TurnTileDiscard riichi tile -> out $ "Discarded " <> pshow tile <> "." <> if riichi then " Riichi!" else ""
    TurnTileDraw dead mt        -> out $ "Draw " <> maybe "" ((<> " ") . pshow) mt <> if dead then "from wanpai." else ""
    TurnAnkan tile              -> out $ "Called " <> pshow tile <> " ankan."

-- ** Sub events

beginTurn :: Player -> ClientM ()
beginTurn p = do
    out "--------------------------------------------"
    Just game <- rview clientGame
    if _playerPlayer game == p
        then out "It's your turn!" >> beginMyTurn
        else out $ "It's turn of " <> pshow p

beginMyTurn :: ClientM ()
beginMyTurn = emit $ TurnTileDraw False Nothing

-- * Auxilary functions

isMyNick :: Nick -> ClientM Bool
isMyNick nick = liftM (\c -> getNick c == nick) $ view clientConn

discardOptions :: ClientM [(Tile, Bool)] -- ^ (discard, can riichi?)
discardOptions = do
    game <- rview clientGame <&> (^?! _Just)
    let tiles = game^.playerMyHand.handConcealed ++ maybeToList (game^.playerMyHand.handPick)
    return $ map (,True) tiles -- TODO check if can riichi

lastDiscard :: ClientM (Maybe (Player, (Tile, UTCTime)))
lastDiscard = (>>= riichiLastDiscard) `liftM` rview clientGame

riichiLastDiscard :: GamePlayer -> Maybe (Player, (Tile, UTCTime))
riichiLastDiscard = do
    pl <- view $ playerPublic.riichiTurn
    mdt <- preview $ playerPublicHands.at pl._Just.handTurnDiscard
    return $ (pl,) <$> join mdt

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

-- ^ "discardTile nth with_riichi"
discardTile :: Int -> Bool -> ClientM ()
discardTile n riichi = do
    tiles <- discardOptions
    case tiles ^? traversed.index n of
        Nothing             -> out $ "No tile for " <> tshow n
        Just (tile, can_riichi)
            | False <- can_riichi
            , True <- riichi -> out "Cannot riichi with that tile"
            | otherwise     -> emit $ InGameAction $ GameTurn $ TurnTileDiscard riichi tile

chatMessage :: ClientM ()
chatMessage = withParam "say: " send
    where
        send ""  = return ()
        send msg = emit $ Message "" msg

-- * Printing

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
    lounge <- rview clientLounge
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
printGameState = rview clientGame >>= out . maybe "Round is not active!" pshow
