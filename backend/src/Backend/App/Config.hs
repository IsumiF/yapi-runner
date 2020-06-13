{-# LANGUAGE StrictData #-}

module Backend.App.Config
  ( Config(..)
  , loadConfig
  ) where

import           Backend.Persist.Config
import           Backend.Util           (aesonOptions)
import           Data.Aeson
import           Data.Yaml
import           RIO
import           RIO.FilePath
import           RIO.Process
import qualified RIO.Text               as T

newtype Config = Config
  { _config_sqlite :: SqliteConfig
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

loadConfig :: HasProcessContext env => RIO env Config
loadConfig = do
    confDir <- fromMaybe "conf" <$> lookupEnvFromContext "CONF_DIR"
    env <- fromMaybe "dev" <$> lookupEnvFromContext "ENV"
    let confPath = T.unpack confDir </> ("config." <> T.unpack env <> ".yaml")
    resultEither <- liftIO $ decodeFileEither confPath
    case resultEither of
      Left err     -> error (show err)
      Right result -> pure result
