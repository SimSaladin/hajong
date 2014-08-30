module Hajong.Game.Round where

import qualified Data.Map as Map
import           Data.Maybe (fromJust)
-- import Control.Monad.RWS
import System.Random.Shuffle (shuffleM)

import Hajong.Game.Tiles
import Hajong.Game.Mentsu
import Hajong.Game.Hand

-- TODO: separate to Hajong.Game.Points or smth
data RoundResults = RoundTsumo { winners :: [Player], payers :: [Player] }
                  | RoundRon   { winners :: [Player], payers :: [Player] }
                  | RoundDraw  { winners :: [Player], payers :: [Player] }
                  deriving (Show, Read)

-- TODO: separate to Hajong.Game.Points or smth
type Points = Int

type RiichiPlayers playerID = [(Player, playerID, Points)]

-- | This would be better renamed as RoundState, as that's what it really is.
data RiichiState = RiichiState
                 { _riichiSecret :: RiichiSecret
                 , _riichiPublic :: RiichiPublic
                 , _riichiEvents :: [GameEvent]
                 } deriving (Show, Read)

data RiichiSecret = RiichiSecret
                 { _riichiWall :: [Tile]
                 , _riichiWanpai :: [Tile]
                 , _riichiHands :: Map Player Hand
                 , _riichiWaitShoutsFrom :: [Player]
                 } deriving (Show, Read)

data RiichiPublic = RiichiPublic
                 { _riichiDora :: [Tile]
                 , _riichiWallTilesLeft :: Int
                 , _riichiRound :: Kazehai
                 , _riichiDealer :: Player
                 , _riichiTurn :: Player
                 , _riichiPlayers :: RiichiPlayers Text
                 , _riichiResults :: Maybe RoundResults
                 } deriving (Show, Read)

-- | State of single player. Note that there is no RiichiSecret.
data GamePlayer = GamePlayer
                { _playerPlayer :: Player -- ^ Me
                , _playerPublic :: RiichiPublic
                , _playerPublicHands :: Map Player HandPublic
                , _playerPlayers :: RiichiPlayers Text
                , _playerMyHand :: Hand
                } deriving (Show, Read)

data TurnAction = TurnTileDiscard Bool Tile -- ^ Riichi?
                | TurnTileDraw Bool (Maybe Tile) -- ^ From wanpai? - sensitive!
                | TurnAnkan Tile
                deriving (Show, Read)

data GameEvent = RoundPrivateStarts GamePlayer -- ^ Only at the start of a round
               | RoundPrivateWaitForShout Int -- ^ Number of seconds left to shout or confirm an ignore (See @GameDontCare@)
               | RoundPrivateChange Player Hand
               | RoundTurnBegins Player
               | RoundTurnAction Player TurnAction
               | RoundTurnShouted Player Shout -- ^ Who, Shout
               | RoundHandChanged Player HandPublic
                        -- TODO this is a bit too vague to be exactly
                        -- useful for clients.. perhaps could identify
                        -- between draws, kans, shouts.
               | RoundEnded RoundResults
               deriving (Show, Read)

data GameAction = GameTurn TurnAction
                | GameShout Shout
                | GameDontCare -- ^ About shouting last discarded tile
                deriving (Show, Read)

makeLenses ''RiichiSecret
makeLenses ''RiichiPublic
makeLenses ''RiichiState
makeLenses ''GamePlayer

-- | Context of game and deal flow.
--
-- Note that RiichiSecret is freely modifiable as a state, but
-- @RiichiPublic@ is read-only. Modifications to the public part must
-- be encoded in RoundEvents. This way it is trivial to keep clients'
-- public states in sync with minimum bandwidth.
type RoundM m =
    ( MonadReader RiichiPublic m
    , MonadState RiichiSecret m
    , MonadWriter [GameEvent] m
    , MonadError Text m
    , Functor m
    , Applicative m
    )

-- * Initialize

defaultPlayers :: [Player]
defaultPlayers = [Player Ton .. Player Pei]

-- | Four-player riichi game
newPublic :: RiichiPublic
newPublic = RiichiPublic
    { _riichiDora          = []
    , _riichiWallTilesLeft = 0
    , _riichiRound         = Ton
    , _riichiDealer        = Player Ton
    , _riichiTurn          = Player Ton
    , _riichiPlayers       = zip3 defaultPlayers (repeat "") (repeat 25000)
    , _riichiResults       = Nothing
    }

newSecret :: IO RiichiSecret
newSecret = liftM dealTiles $ shuffleM riichiTiles
    where
        dealTiles tiles = RiichiSecret
            { _riichiWall           = wall
            , _riichiWanpai         = wanpai
            , _riichiHands          = Map.fromList $ zip defaultPlayers (map initHand [h1, h2, h3, h4])
            , _riichiWaitShoutsFrom = []
            } where
                (hands, xs)             = splitAt (13 * 4) tiles
                ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
                (wanpai, wall)          = splitAt 14 xs

-- | New state with first round ready to start. Convenient composite of
-- newPublic, newSecret and setSecret.
newRiichiState :: IO RiichiState
newRiichiState = liftM (`setSecret` newPublic) newSecret

setSecret :: RiichiSecret -> RiichiPublic -> RiichiState
setSecret secret public = RiichiState
    (secret & set riichiWanpai wanpai')
    (public & set riichiDora [dora] & set riichiWallTilesLeft (secret ^. riichiWall.to length))
    []
    where
        (dora : wanpai') = secret ^. riichiWanpai

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound rs = do
    secret <- newSecret
    return $ setSecret secret $ (rs ^. riichiPublic) & set riichiTurn (rs ^. riichiPublic.riichiDealer)

-- * Round logic (RoundM actions)

startRound :: RoundM m => m ()
startRound = do
    view riichiPlayers >>= mapM (getPlayerState . view _1) >>= tell . map RoundPrivateStarts
    view riichiTurn >>= tell . return . RoundTurnBegins

-- | Attempt to run a @TurnAction@ as the given user. Fails if it is not
-- his turn.
runTurn :: RoundM m => Player -> TurnAction -> m ()
runTurn p ta = do
    view riichiTurn >>= (`when` throwError "Not your turn") . (/= p)
    publishTurnAction p ta
    handOf' p >>= run >>= updateHand p
    where
        run = case ta of
            TurnTileDiscard True  tile -> discardRiichi tile -- TODO move to next player
            TurnTileDiscard False tile -> discard tile
            TurnAnkan tile             -> ankanOn tile
            TurnTileDraw False _       -> drawWall
            TurnTileDraw True  _       -> drawDeadWall

runShout :: RoundM m => Shout -> Player -> m ()
runShout shout shouter = do
    turn <- view riichiTurn
    (m, hand) <- shoutFromHand shout =<< handOf' turn
    updateHand turn hand

    handOf' shouter >>= meldTo shout m >>= updateHand shouter
    tell [RoundTurnShouted shouter shout]

-- | If win(s) were declared, wall was exhausted or four kans were declared
-- (and TODO declarer is not in the yakuman tenpai): return "RoundResults".
--
-- Otherwise the turn is passed to next player as if the player in turn
-- discarded previously.
advanceAfterDiscard :: RoundM m => m (Maybe RoundResults)
advanceAfterDiscard = do
    tilesLeft <- view riichiWallTilesLeft
    doras     <- view riichiDora
    case () of
        _ | tilesLeft == 0 || length doras == 5 -> Just <$> roundEndedDraw
          | otherwise                           -> do
                turnOf <- view riichiTurn
                tell [RoundTurnBegins $ nextPlayer turnOf]
                return Nothing

-- * Auxilary

advanceAuto :: RiichiState -> TurnAction
advanceAuto = TurnTileDiscard False . handAutoDiscard . fromJust <$>
    (view (riichiPublic.riichiTurn) >>= view . (riichiSecret . ) . handOf)

nextPlayer :: Player -> Player
nextPlayer = toEnum . (`mod` 4) . (+ 1) . fromEnum

handOf :: Player -> Lens RiichiSecret RiichiSecret (Maybe Hand) (Maybe Hand)
handOf player = riichiHands.at player

handOf' :: RoundM m => Player -> m Hand
handOf' player = use (handOf player) >>= maybe (throwError "Player not found") return

-- | Build the player's "@GamePlayer@" record, or the state of the game as
-- seen by the player (hide "RiichiSecret" but show the player's own hand).
getPlayerState :: RoundM m => Player -> m GamePlayer
getPlayerState p = GamePlayer
    <$> pure p
    <*> view id
    <*> use (riichiHands.to (map _handPublic))
    <*> view riichiPlayers
    <*> use (riichiHands.at p.to fromJust)

applyGameEvents' :: [GameEvent] -> RiichiPublic -> RiichiPublic
applyGameEvents' evs rp = _playerPublic $ applyGameEvents evs $ GamePlayer
    (error "Not accessed")
    rp mempty mempty $ Hand [] Nothing Nothing $ HandPublic [] [(error "N/A", Nothing)] False Nothing

applyGameEvents :: [GameEvent] -> GamePlayer -> GamePlayer
applyGameEvents evs gp = foldr applyGameEvent gp evs

applyGameEvent :: GameEvent -> GamePlayer -> GamePlayer
applyGameEvent ev = case ev of
    RoundTurnBegins p        -> playerPublic.riichiTurn .~ p
    RoundTurnAction p ta     -> applyTurnAction p ta
    RoundTurnShouted p shout ->
        over (playerPublicHands.at p._Just.handOpen) (|> fromShout shout)
        . set (playerPublic.riichiTurn) p
        . set (playerPublicHands.at (shoutedFrom shout)._Just.handDiscards._last._2) (Just p)
    RoundHandChanged p hp    -> playerPublicHands.at p._Just .~ hp
    RoundEnded how           -> playerPublic.riichiResults .~ Just how
    RoundPrivateChange _ h   -> playerMyHand .~ h
    RoundPrivateStarts gp    -> const gp

applyTurnAction :: Player -> TurnAction -> GamePlayer -> GamePlayer
applyTurnAction p ta = case ta of
    TurnTileDiscard riichi tile ->
        over (playerPublicHands.at p._Just.handDiscards) (|> (tile, Nothing))
        . set (playerPublicHands.at p._Just.handRiichi) riichi
    TurnTileDraw _ _  -> playerPublic.riichiWallTilesLeft -~ 1
    TurnAnkan tile    -> over (playerPublicHands.at p._Just.handOpen) (|> kantsu (replicate 4 tile))

-- * Operations

-- | Set the hand of player
updateHand :: RoundM m => Player -> Hand -> m ()
updateHand p new = do
    old <- handOf' p
    handOf p ?= new
    when (old /= new) $ tell [ RoundPrivateChange p new ]
    when (_handPublic old /= _handPublic new) $ tell [RoundHandChanged p $ _handPublic new]

-- | Turn a possibly sensitive TurnAction to a non-sensitive (fully public)
-- RoundEvent.
publishTurnAction :: RoundM m => Player -> TurnAction -> m ()
publishTurnAction player ra = tell $ case ra of
    TurnTileDraw b _ -> [ RoundTurnAction player $ TurnTileDraw b Nothing ]
    _                -> [ RoundTurnAction player ra ]

-- ** Get results

roundEndedDraw :: RoundM m => m RoundResults
roundEndedDraw =
    roundEndsWith $ RoundDraw [] [] -- TODO tenpai players

roundEndsWith :: RoundM m => RoundResults -> m RoundResults
roundEndsWith results = do
    tell [RoundEnded results]
    return results

-- ** Player in turn

drawWall :: RoundM m => Hand -> m Hand
drawWall hand = do
    wall <- use riichiWall
    case wall of
        (x:xs) -> do riichiWall .= xs
                     return $ set handPick (Just x) hand
        _ -> throwError "No tiles left"

drawDeadWall :: RoundM m => Hand -> m Hand
drawDeadWall hand = preuse (riichiWall._Snoc)
    >>= maybe (throwError "No tiles in wall!") (draw . fst)
    where
        draw wall = do
            riichiWall .= wall
            Just (dt, wanpai) <- preuse (riichiWanpai._Cons)
            riichiWanpai .= wanpai
            return $ handPick .~ Just dt $ hand

-- | Set riichiWaitShoutsFrom to players who could shout the discard.
endTurn :: RoundM m => Tile -> m ()
endTurn dt = do
    hands <- Map.filter (not . null . shoutsOn dt) <$> use riichiHands
    riichiWaitShoutsFrom .= Map.keys hands
