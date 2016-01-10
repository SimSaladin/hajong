{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Game.Mechanics
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This is the next abstraction above "Mahjong.Round".
------------------------------------------------------------------------------
module Mahjong.Round where

------------------------------------------------------------------------------
import           Import
import           Mahjong.Configuration
import           Mahjong.Kyoku
------------------------------------------------------------------------------
import           Control.Monad.RWS
import qualified Data.Map as Map
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Data.UUID as UUID
------------------------------------------------------------------------------

-- * GameState

-- | "GameState" records all information of a single game.
data GameState playerID = GameState
                   { _gameKyoku    :: Maybe Kyoku           -- ^ Active kyoku
                   , _gameHistory  :: [Kyoku]               -- ^ In decending order
                   , _gamePlayers  :: Map Player playerID
                   , _gameUUID     :: UUID.UUID
                   , _gameSettings :: GameSettings
                   } deriving (Show, Typeable, Read, Functor, Generic)

--  TODO this instance is impaired, doesn't show everything it should.
instance P.Pretty p => P.Pretty (GameState p) where
    pretty GameState{..} = P.pretty (show _gameSettings) P.<$$>
                           P.pretty (UUID.toString $ _gameUUID) P.<$$>
                           P.prettyList (Map.elems _gamePlayers) P.<$$>
                           P.prettyList (map P.pretty _gameHistory) P.<$$>
                           P.pretty _gameKyoku

-- | Create a new GameState with the given label.
newEmptyGS :: a -> UUID.UUID -> GameSettings -> GameState a
newEmptyGS defPlayer = GameState Nothing [] (Map.fromList $ zip fourPlayers $ repeat defPlayer)

-- ** Lenses

--
makeLenses ''GameState

------------------------------------------------------------------------------

-- * RoundM

-- | Concrete instance of "Mahjong.Round.RoundM".
type RoundM = RWST Kyoku [GameEvent] Kyoku (Either Text)

-- | Execute a round action in the "GameState".
--
-- Succesfull return value contains the value from the run "RoundM" action,
-- arbitrarily modified "RiichiSecret" and public changes encoded in
-- "GameEvents".
--
-- RoundM-actions do not explicitly modify the public state (RiichiPublic),
-- so **it is important you apply the changes implied by the events on the
-- state!** Haskell clients may use "@applyRoundEvents@".
runRoundM :: RoundM r -> GameState p -> Either Text (r, Kyoku, [GameEvent])
runRoundM m gs = maybe (Left "No active round!") (flip runKyoku m) (_gameKyoku gs)

-- | Run action and apply gameveents.
runKyoku :: Kyoku -> RoundM a -> Either Text (a, Kyoku, [GameEvent])
runKyoku k m = fmap logEvents $ runRWST m k k
    where logEvents (res, k', evs) = (res, k' & sEventHistory %~ mappend evs, evs)

------------------------------------------------------------------------------

-- * Players

-- | This class defines characteristics of a player seat.
class Eq playerID => IsPlayer playerID where

    -- | Is this player seat reserved.
    seatOccupied :: playerID -> Bool

    -- | first argument is a seat for the second.
    seatOf :: playerID -> playerID -> Bool

    playerReady  :: playerID -> Bool

    playerNick   :: playerID -> Text

------------------------------------------------------------------------------

-- * Rounds

-- | If appropriate, begin the game
maybeBeginGame :: IsPlayer p => GameState p -> Maybe (IO (GameState p))
maybeBeginGame gs = do
    guard . isNothing       $ gs^.gameKyoku
    guard . (== 4) . length $ gs^.gamePlayers
    guard . null            $ gs^.gamePlayers^..each.filtered (not . playerReady)
    return $ do
        rs <- newKyoku fourPlayers (gs^.gamePlayers^..each.to playerNick)
        return $ gameKyoku .~ Just rs $ gs

-- * Modify

-- | Try putting the given client to its previous place ior an empty seat.
-- Returns Nothing if the game is already full.
addClient :: IsPlayer p => p -> GameState p -> Maybe (GameState p)
addClient client gs = do p <- clientToPlayer client gs
                         return $ gamePlayers.at p .~ Just client $ gs

setClient :: IsPlayer p => p -> Player -> GameState p -> GameState p
setClient client p = gamePlayers.at p .~ Just client

-- * Query

playerToClient :: GameState p -> Player -> Maybe p
playerToClient gs p = gs^.gamePlayers.at p

-- | Given a client, find either its old place, or assign it to an empty
-- seat.
clientToPlayer :: IsPlayer p => p -> GameState p -> Maybe Player
clientToPlayer c gs = fmap (^._1) $
    (gs^.gamePlayers & ifind (\_ s -> s `seatOf` c)) -- previous seat
    `mplus`
    (gs^.gamePlayers & ifind (\_ s -> not (seatOccupied s))) -- empty seat
