------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Server.Internal
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This module defines the @Server@-monad and the associated server state,
-- @ServerSt@.
--
------------------------------------------------------------------------------
module Hajong.Server.Internal where

------------------------------------------------------------------------------
import           Mahjong
import           Hajong.Server.Config
import           Hajong.Database
import           Hajong.Client
import           Hajong.Worker
------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Control.Monad.Trans.Control        (MonadBaseControl(..))
import           Control.Monad.Base                 (MonadBase)
import           Control.Concurrent
import           Data.Acid hiding (update, query)
import qualified Data.Acid as Acid
import           System.Log.FastLogger (LoggerSet, pushLogStr)
------------------------------------------------------------------------------

-- | Server logic monad.
newtype Server a = Server { unServer :: ReaderT ServerSt IO a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadReader ServerSt, MonadBase IO )

data ServerSt = ServerSt
    { _db              :: AcidState ServerDB
    , _serverConf      :: ServerConfig
    , _seConnections   :: TVar (IntMap Client)      -- ^ Everyone connected
    , _seLounge        :: TVar (Set Client)         -- ^ Lounge
    , _seWatcher       :: TChan (Int, WorkerResult) -- ^ Worker watcher, **broadcast chan**.
    , _seLoggerSet     :: LoggerSet                 -- ^ Fed to new workers
    , _seClient        :: Client                    -- ^ When serving a single client
    , _seWorkers       :: TVar (IntMap RunningGame) -- ^ Running workers
    } deriving (Typeable)

-- | Game worker thread, see @Worker@.
data RunningGame = RunningGame
    { _gWorker         :: WorkerData
    , _gThread         :: ThreadId
    , _gClients        :: IntMap Client
    } deriving (Typeable)

-- * Lenses
makeLenses ''RunningGame
makeLenses ''ServerSt

-- Instances

instance MonadBaseControl IO Server where
    type StM Server a = a
    liftBaseWith f    = Server $ liftBaseWith $ \q -> f (q . unServer)
    restoreM          = Server . restoreM

instance MonadLogger Server where
    monadLoggerLog loc src lvl msg = do
        lgr <- view seLoggerSet
        let out = defaultLogStr loc src lvl (toLogStr msg)
        liftIO $ pushLogStr lgr out

-- * Executing Server actions

runServer :: ServerSt -> Server a -> IO a
runServer st ma = runReaderT (unServer ma) st

runClient :: Client -> ServerSt -> Server a -> IO a
runClient client st = runServer (st&seClient.~client)

withClient :: Client -> Server a -> Server a
withClient c = local (seClient.~c)

-- * Database

update :: (UpdateEvent event, EventState event ~ ServerDB) => event -> Server (EventResult event)
update ev = view db >>= \d -> liftIO (Acid.update d ev)

query :: (QueryEvent event, EventState event ~ ServerDB) => event -> Server (EventResult event)
query ev = view db >>= \d -> liftIO (Acid.query d ev)
