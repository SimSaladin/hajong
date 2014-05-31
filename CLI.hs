{-# LANGUAGE RankNTypes, OverloadedStrings, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module CLI where

import           ClassyPrelude hiding (finally, handle, toLower)
import           Control.Lens
import           Control.Concurrent (forkFinally, killThread, myThreadId, ThreadId)
import           Control.Monad.Reader (runReaderT, ReaderT, MonadReader(..))
import           Data.List (elemIndex)
import           Data.Char (isUpper, toLower)
import qualified Data.Text as T
import           System.Random
import           System.Console.Haskeline
import qualified Network.WebSockets as WS

import           GameTypes
import           Riichi
import           Server hiding (Client) -- TODO it shouldn't even export this
import           PrettyPrint

-- * Types

data ClientState = ClientState
                 { _clientConn         :: WS.Connection
                 , _clientNick         :: Text
                 , _clientMainThread   :: ThreadId
                 , _clientLounge       :: MVar Lounge
                 , _clientGame         :: MVar (Maybe (GamePlayer Nick))
                 , _clientReceiveChan  :: TChan Text
                 , _clientCanInterrupt :: MVar Bool
                 , _clientWaiting      :: MVar Int
                 }

makeLenses ''ClientState

newClientState :: WS.Connection -> Text -> IO ClientState
newClientState conn nick = ClientState conn nick
    <$> myThreadId
    <*> newMVar (Lounge mempty mempty)
    <*> newMVar Nothing
    <*> newTChanIO
    <*> newMVar True
    <*> newMVar (-1)

-- ** Typeclass hackery

type Client a = ClientOutput m => m a
type UI     a = ClientInput m => m a

class ( Functor m, Applicative m, Monad m
      , MonadIO m
      , MonadReader ClientState m
      ) => ClientOutput m where
    emit :: Event -> m ()
    emit ev = view clientConn >>= (`unicast` ev)
    outAll :: [Text] -> m ()
    out :: Text -> m ()
    out x = outAll [x]

class ClientOutput m => ClientInput m where
    askChar     :: Text -> m (Maybe Char)
    withParam   :: Text -> (Text -> m ()) -> m ()

-- *** Real console input and output

consoleInput :: InputT (ReaderT ClientState IO) ()
consoleInput = withInterrupt loop
    where
        loop = handleInterrupt interrupt (inputLoop loop)
        interrupt = do
            chan <- view clientReceiveChan
            mline <- liftIO $ atomically $ tryReadTChan chan
            maybe loop (\line -> out line >> interrupt) mline

instance ClientOutput (InputT (ReaderT ClientState IO)) where
    outAll = mapM_ (outputStrLn . unpack)
instance ClientInput (InputT (ReaderT ClientState IO)) where
    askChar = getInputChar . unpack
    withParam prompt ma =
        uninterrupted $ do
            line <- getInputLine (unpack prompt) <&> fmap pack
            maybe (return ()) ma line
      where
        uninterrupted f = do
            ivar <- view clientCanInterrupt
            (liftIO (swapMVar ivar False) >> f) `finally`
                -- throw interrupt to print stuff from queue after the action.
                (liftIO (swapMVar ivar True)
                >> view clientMainThread >>= liftIO . (`throwTo` Interrupt))
instance MonadReader ClientState (InputT (ReaderT ClientState IO)) where
    ask = lift ask
    local _ _ = error "local not implemented"

-- *** Real listener instance

instance ClientOutput (ReaderT ClientState IO) where
    -- Print to main thread's haskeline via a chan from any thread.
    outAll xs = do
        chan <- view clientReceiveChan
        mapM_ (liftIO . atomically . writeTChan chan) xs

        interrupt <- rview clientCanInterrupt
        when interrupt $ view clientMainThread >>= liftIO . (`throwTo` Interrupt)


-- * App

-- | Run client with console IO.
clientMain :: Maybe Handle -> IO ()
clientMain mhandle = clientMain' input listener
    where
        listener    = runReaderT clientReceiver
        input state = flip runReaderT state $ runInputTBehavior
            (maybe defaultBehavior useFileHandle mhandle)
            defaultSettings
            consoleInput

-- | Run client with given IO functions.
clientMain' :: (ClientState -> IO ()) -> (ClientState -> IO ()) -> IO ()
clientMain' input = WS.runClient "localhost" 9160 "/" . clientApp input

clientApp :: (ClientState -> IO ()) -- ^ Input loop
          -> (ClientState -> IO ()) -- ^ Server listener
          -> WS.ClientApp ()
clientApp input listener conn = do

    -- Assign a nick
    ident <- liftM pack $ replicateM 5 $ randomRIO ('a', 'z')

    state <- newClientState conn ident

    -- Start server listener
    listenerMVar <- newEmptyMVar 
    listenerThread <- (takeMVar listenerMVar >> listener state) `forkFinally` const (putMVar listenerMVar ())

    -- Tell we have joined
    unicast conn (JoinServer ident)

    -- Run input loop; the mvar is used to prevent listener from throwing
    -- interrupts before input can handle them.
    putMVar listenerMVar () >> input state

    -- cleanup
    WS.sendClose conn (PartServer "Bye")
    _ <- killThread listenerThread >> takeMVar listenerMVar
    return ()


-- * User input

inputLoop :: ClientInput m => m () -> m ()
inputLoop loop = do
    minput <- askChar =<< shortStatus
    case minput of
        Nothing    -> loop
        Just 'q'   -> return ()
        Just input -> inputHandler input >> loop

inputHandler :: ClientInput m => Char -> m ()
inputHandler '?' = printHelp
inputHandler '!' = printStatus
inputHandler ' ' = chatMessage
inputHandler  x  = do
    inGame <- rview clientGame
    if isJust inGame then gameHandler x else loungeHandler x
    where
        loungeHandler 'n' = printUsers
        loungeHandler 'g' = printGames
        loungeHandler 'c' = createGame
        loungeHandler 'j' = joinGame
        loungeHandler 'r' = rawCommand
        loungeHandler  _  = unknownCommand

        gameHandler 'p' = undefined
        gameHandler 'c' = undefined
        gameHandler 'k' = undefined
        gameHandler 'r' = undefined
        gameHandler 'l' = undefined
        gameHandler  _  = case elemIndex (toLower x) discardKeys of
                              Nothing -> unknownCommand
                              Just n -> discardTile n (isUpper x)

        discardKeys = "aoeuidhtns-mwvz"

        unknownCommand = out $ "Command `" <> pack [x] <> "` not recognized"

shortStatus :: ClientOutput m => m Text
shortStatus = do
    nick   <- view clientNick
    inGame <- rview clientGame
    gameN  <- rview clientWaiting
    let status = if gameN >= 0
                     then maybe "wait" (const "game") inGame <> " " <> tshow gameN
                     else "idle"
    return $ "(" <> status <> ") <" <> nick <> "> "

discardTile :: ClientInput m => Int -> Bool -> m ()
discardTile n riichi = do
    game <- rview clientGame <&> (^?! _Just)
    let tiles = game^.playerMyHand.handConcealed ++ maybeToList (game^.playerMyHand.handPick)
    case tiles ^? traversed.index n of
        Nothing   -> out $ "No tile at index " <> tshow n
        Just tile -> emit $ GameAction $ ($ tile) $ if riichi then TurnRiichi else TurnDiscard

printHelp :: ClientInput m => m ()
printHelp = outAll
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

printStatus :: ClientInput m => m ()
printStatus = do
    mgame       <- rview clientGame
    gameWait    <- rview clientWaiting
    case mgame of
        Nothing -> out $ "In lounge" <> if gameWait >= 0 then ", ready for game n. " <> tshow gameWait else ""
        Just _  -> out $ "In game n." <> tshow gameWait

chatMessage :: ClientInput m => m ()
chatMessage = withParam "say: " send
    where
        send ""  = return ()
        send msg = emit $ Message "" msg

createGame :: ClientInput m => m ()
createGame = withParam "Game name: " $ \name ->
        let name' = T.dropWhileEnd (== ' ') $ T.dropWhile (== ' ') name
            in case name' of
                 "" -> out "Name cannot be empty"
                 _  -> emit $ CreateGame name'

joinGame :: ClientInput m => m ()
joinGame = printGames >> withParam "join game: " go
    where
        go str = case readMay str of
            Nothing -> out $ "NaN: " <> str
            Just n  -> emit $ JoinGame n ""

rawCommand :: ClientInput m => m ()
rawCommand = withParam "send command: " go
    where
        go str = case readMay str of
            Nothing -> out "Command not recognized"
            Just cmd -> emit cmd


-- * Server listener

clientReceiver :: ClientOutput m => m ()
clientReceiver = do
    out "Connected to server. Type ? for help."
    conn <- view clientConn
    forever $ liftIO (WS.receiveData conn) >>= clientEventHandler

clientEventHandler :: Event -> Client ()
clientEventHandler ev = case ev of
    JoinServer nick   -> nickJoined nick
    PartServer nick   -> nickParted nick
    LoungeInfo lounge -> rswap clientLounge lounge >> printLounge
    NewGame info      -> gameCreated info
    StartGame gstate  -> rswap clientGame (Just gstate) >> startGame
    JoinGame n nick   -> handleJoinGame n nick 
    GameAction ta     -> handleTurnAction ta
    GameShout shout   -> handleGameShout shout
    Message sayer msg -> out $ "<" <> sayer <> "> " <> msg
    Invalid err       -> out $ "[error] " <> err
    x                 -> out $ "Received an unhandled event: " <> tshow x

nickJoined :: Text -> Client ()
nickJoined nick = do
    me <- view clientNick
    if me == nick
        then out "Succesfully joined server."
        else do
            rmodify clientLounge $ return . over loungeNicksIdle (insertSet nick)
            out $ nick <> " has joined."

nickParted :: Text -> Client ()
nickParted nick = do
    n <- view clientNick
    if n == nick
        then out "You have left the server"
        else do
            rmodify clientLounge $ return
                . over loungeNicksIdle (deleteSet nick)
                . over (loungeGames.each._2) (deleteSet nick)
            out $ nick <> " has parted."

gameCreated :: (Int, Text, Set Nick) -> Client ()
gameCreated (n, name, nicks) = do
    out $ "New game `" <> ppGame n (name, nicks)
    rmodify clientLounge $ return . over loungeGames (insertMap n (name, nicks))

handleJoinGame :: Int -> Text -> Client ()
handleJoinGame n nick = do
    rmodify clientLounge $ return
        . over (loungeGames.at n.traversed._2) (insertSet nick)
        . over loungeNicksIdle (deleteSet nick)

    lounge <- rview clientLounge
    let count = length $ view (loungeGames.at n.traversed._2) lounge
        countInfo
            | count < 4 = tshow (4 - count) <> " more players until game starts." 
            | otherwise = " Game is starting!"

    me <- liftM (== nick) $ view clientNick
    if me
        then do _ <- rswap clientWaiting n
                out $ "Joined the game (" <> tshow n <> "). " <> countInfo
        else out $ nick <> " joined game (" <> tshow n <> "). " <> countInfo

handleTurnAction :: TurnAction -> Client ()
handleTurnAction ta = do
    case ta of
        TurnRiichi _          -> undefined
        TurnDiscard tile      -> undefined
        TurnDraw dead _       -> undefined
        TurnAnkan tile        -> undefined
        TurnShouted shout who -> undefined

handleGameShout :: Shout -> Client ()
handleGameShout shout = undefined

-- * Printing

startGame :: Client ()
startGame = do
    out "Entering game!"
    printGameState

printUsers :: Client ()
printUsers = do
    lounge <- view clientLounge >>= liftIO . readMVar
    outAll [ "Users idle: " <> ppNicks (lounge ^. loungeNicksIdle)]

printGames :: Client ()
printGames = do
    lounge <- rview clientLounge
    case lounge^.loungeGames & imap ppGame & toListOf each of
       []     -> out "No games"
       (x:xs) -> outAll $ "Games: " <> x : map ("       " <>) xs

printLounge :: Client ()
printLounge = printUsers >> printGames

printGameState :: Client ()
printGameState = do
    Just game <- rview clientGame
    let public = game ^. playerPublic
    out $ pshow game

-- * PP

ppGame :: Int -> (Text, Set Nick) -> Text
ppGame n (name,nicks) = mconcat ["(", tshow n, ") ", name, " [", ppNicks nicks, "]"]

ppNicks :: Set Nick -> Text
ppNicks nicks = case setToList nicks of
            [] -> ""
            (x:xs) -> foldl' (\a b -> a <> ", " <> b) x xs


-- * Helpers

rview :: (MonadReader s m, MonadIO m) => Getting (MVar b) s (MVar b) -> m b
rview l = view l >>= liftIO . readMVar

rswap :: (MonadReader s m, MonadIO m) => Getting (MVar b) s (MVar b) -> b -> m b 
rswap l a = view l >>= liftIO . (`swapMVar` a)

rmodify :: (MonadReader s m, MonadIO m) => Getting (MVar a) s (MVar a) -> (a -> IO a) -> m ()
rmodify l f = view l >>= liftIO . (`modifyMVar_` f)
