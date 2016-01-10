------------------------------------------------------------------------------
-- |
-- Module         : Hajong.Server.Config
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Game server configuration.
------------------------------------------------------------------------------
module Hajong.Server.Config where

import Import
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), (.!=))
import Data.Yaml (decodeFileEither, parseEither)
import System.Exit (exitFailure)

data ServerConfig = ServerConfig
    { _websocketHost :: String
    -- ^ Host to use for incoming websocket clients. Default to 0.0.0.0
    , _websocketPort :: Int
    -- ^ Port to use for incoming websocket clients.
    , _databaseSocket :: FilePath
    -- ^ Provide access to the ACIDic database at this socket.
    , _logDirectory :: FilePath
    -- ^ Directory to store logs into
    , _websocketCtrlSecret :: Text
    -- ^ A secret key a client must know to gain access into the internal
    -- control channel via WS.
    } deriving (Show, Read, Eq, Typeable)

instance FromJSON ServerConfig where
    parseJSON = withObject "ServerConfig" $ \o -> do
        _websocketHost       <- o .:? "websocket-addr" .!= "0.0.0.0"
        _websocketPort       <- o .: "websocket-port"
        _websocketCtrlSecret <- o .: "ctrl-secret-ws"
        _databaseSocket      <- o .: "db-socket-file"
        _logDirectory        <- o .: "logs-directory"
        return ServerConfig{..}

-- * Reading configuration

-- | Exits when the configuration could not be read.
readConfigFile :: FilePath
               -> Maybe Text -- ^ Optional subsection to use as the root.
               -> IO ServerConfig
readConfigFile file mobj = do
    val <- decodeFileEither file >>= either (\err -> print err >> exitFailure) return
    either (\err -> print err >> exitFailure) return $
        parseEither (maybe parseJSON (\k -> withObject "ServerConfig" (.: k)) mobj) val

-- * Lenses

makeLenses ''ServerConfig
