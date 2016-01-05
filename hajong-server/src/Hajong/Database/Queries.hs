{-# LANGUAGE MultiWayIf #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Hajong.Database.Queries
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- File Created   : 2016-01-03T20:30:37+0200
------------------------------------------------------------------------------
module Hajong.Database.Queries where

import           Hajong.Database.Types
------------------------------------------------------------------------------
import           Mahjong
------------------------------------------------------------------------------
import           Data.Acid
import           Data.ReusableIdentifiers
import           Data.UUID                  (UUID)
import           Data.Time.Clock (secondsToDiffTime)
------------------------------------------------------------------------------

getClientRecord :: Int -> Query ServerDB (Maybe ClientRecord)
getClientRecord i = view (seReserved.at i)

getGames :: Query ServerDB (IntMap Game)
getGames = view seGames

getGame :: Int -> Query ServerDB (Maybe Game)
getGame i = view (seGames.at i)

dumpDB :: Query ServerDB ServerDB
dumpDB = ask

-- * Updates

-- | Add the new client to ServerDB if
--      * its @ident@ is known,
--      * the client knows correct @token@ and
--      * there are no other clients with the @ident@
--
--  @connectClient currentTime ident token@
connectClient :: UTCTime -> Int -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
connectClient time ident token = do
    reserved <- use (seReserved.at ident)
    case reserved of
        Nothing                     -> return $ Left $ "Unknown identity: " <> tshow ident
        Just c | token == c^.cToken -> do let c' = c&cStatus.~Right time
                                          _ <- seReserved.at ident <.= Just c' -- TODO EventResult: should it be checked?
                                          return (Right (ident, c'))
               | otherwise          -> return $ Left $ "Auth tokens didn't match (got " <> token <> ")"

-- | Client disconnects: Set status to Left in corresponding ClientRecord. 
--
-- @partClient currentTime ident@
partClient :: UTCTime -> Int -> Update ServerDB (Maybe ClientRecord)
partClient time ident = use (seReserved.at ident) >>= \case
        Just c  -> seReserved.at ident <.= Just (c&cStatus.~Left time)
        Nothing -> return Nothing

-- | Assign a @ident@ to a new player if possible.
--
-- @registerPlayer nick token isRegistered@
registerPlayer :: Text -> Text -> Bool -> Update ServerDB (Either Text (Int, ClientRecord))
registerPlayer nick token reg = do
    taken <- use (seNicks.at nick)
    rec   <- use sePlayerRecord
    case (taken, newId rec) of
        -- nick is taken
        (Just i, _)         -> do Just c <- use (seReserved.at i)
                                  if | not reg        -> return (Left "Nick is taken")
                                     | c^.cRegistered -> return (Right (i, c))
                                     | otherwise      -> return (Left "Nick is used by someone anonymous") -- TODO overwrite his nick?
        -- server has room
        (_, Just (i, rec')) -> do sePlayerRecord  .= rec'
                                  seNicks.at nick .= Just i
                                  let c = ClientRecord nick token reg (Left $ UTCTime (ModifiedJulianDay 0) $ secondsToDiffTime 0) Nothing
                                  seReserved.at i .= Just c
                                  return (Right (i, c))
        -- server is full
        (_, Nothing)        -> return (Left "Server is full")

-- | Assign an @ident@ to an anonymous player.
--
-- @registerAnonymousPlayer nick token@
registerAnonymousPlayer :: Text -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
registerAnonymousPlayer nick token = registerPlayer nick token False

-- | Assign an @ident@ to a registered player.
--
-- *TODO: If the nick is taken by an anon player, we should force the anon to change his nick.*
--
-- @registerLoggedInPlayer nick token@
registerLoggedInPlayer :: Text -> Text -> Update ServerDB (Either Text (Int, ClientRecord))
registerLoggedInPlayer nick token = use (seNicks.at nick) >>= \case
    Nothing -> registerPlayer nick token True
    Just i  -> do
        Just c <- use (seReserved.at i)
        if c^.cRegistered then return (Right (i, c))
                          else return (Left "Your nick is in use by someone anonymous!")

-- | Set the game of a player
setPlayerGame :: Int -- ^ Game ID
              -> Int -- ^ Client ID
              -> Update ServerDB ()
setPlayerGame gid i = seReserved.at i._Just.cInGame .= Just gid

setGame :: Int -> Game -> Update ServerDB ()
setGame gid game = seGames.ix gid .= game

destroyGame :: Int -> Update ServerDB [ClientRecord]
destroyGame gid = do seGameRecord %= freeId gid
                     cs <- preuse (seGames.at gid._Just.gamePlayers)
                     seGames.at gid .= Nothing
                     rs <- forM (maybe [] (^..each) cs) $ \i -> do
                        seReserved.at i._Just.cInGame .= Nothing
                        use (seReserved.at i)
                     return (catMaybes rs)

insertGame :: Game -> Update ServerDB (Either Text Int)
insertGame game = do
    rec <- use seGameRecord
    case newId rec of
        Just (gid, rec') -> do seGameRecord .= rec'
                               seGames.at gid .= Just game
                               return (Right gid)
        Nothing -> return (Left "Server's Game capacity reached")

logWorkerResult :: PastGame -> Update ServerDB ()
logWorkerResult pg = seHistory.at (_gameUUID $ _pgGameState pg) .= Just pg

flushWorkerLog :: Update ServerDB ()
flushWorkerLog = seHistory .= mempty

getWorkerResultLog :: Query ServerDB (Map UUID PastGame)
getWorkerResultLog = view seHistory

