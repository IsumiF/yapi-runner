{-# LANGUAGE StrictData #-}

module Backend.App.Config
  ( HasConfig(..)
  , MonadConfig(..)
  , MonadBaseUrl(..)
  , Config(..)
  , config_sqlite
  , config_auth
  , loadConfig
  ) where

import qualified Backend.Handler.Config as Handler
import           Backend.Persist.Config
import           Backend.Util           (aesonOptions)
import           Control.Lens           (makeLenses)
import           Data.Aeson
import           Data.Yaml
import           RIO
import           RIO.FilePath
import           RIO.Process
import qualified RIO.Text               as T

class HasConfig env where
  configL :: Lens' env Config

data Config = Config
    { _config_host     :: Text
    , _config_basePath :: Text
    , _config_sqlite   :: SqliteConfig
    , _config_auth     :: Handler.AuthConfig
    }
    deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''Config

loadConfig :: HasProcessContext env => RIO env Config
loadConfig = do
    confDir <- fromMaybe "conf" <$> lookupEnvFromContext "CONF_DIR"
    env <- fromMaybe "dev" <$> lookupEnvFromContext "ENV"
    let confPath = T.unpack confDir </> ("config." <> T.unpack env <> ".yaml")
    resultEither <- liftIO $ decodeFileEither confPath
    case resultEither of
      Left err     -> error (show err)
      Right result -> pure result

class MonadBaseUrl m => MonadConfig m where
  getConfig :: m Config

instance (HasConfig env) => MonadConfig (RIO env) where
  getConfig = view configL

class Monad m => MonadBaseUrl m where
  getHost :: m Text
  getBasePath :: m Text
  getBaseUrl :: m Text
  getBaseUrl = (<>) <$> getHost <*> getBasePath

instance (HasConfig env) => MonadBaseUrl (RIO env) where
  getHost = do
    config <- view configL
    pure $ _config_host config
  getBasePath = do
    config <- view configL
    pure $ _config_basePath config
