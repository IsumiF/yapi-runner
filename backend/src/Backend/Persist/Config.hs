{-# LANGUAGE StrictData #-}

module Backend.Persist.Config where

import           Backend.Util (aesonOptions)
import           Data.Aeson
import           GHC.Generics
import           RIO

data SqliteConfig = SqliteConfig
  { _sqliteConfig_file     :: Text
  , _sqliteConfig_poolSize :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON SqliteConfig where
  parseJSON = genericParseJSON aesonOptions
