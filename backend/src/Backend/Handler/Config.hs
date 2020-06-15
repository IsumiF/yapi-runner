module Backend.Handler.Config
  ( AuthConfig(..)
  , authConfig_clientId
  , authConfig_clientSecret
  ) where

import           Backend.Util (aesonOptions)
import           Control.Lens
import           Data.Aeson
import           GHC.Generics
import           RIO

data AuthConfig = AuthConfig
    { _authConfig_clientId     :: Text
    , _authConfig_clientSecret :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON AuthConfig where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''AuthConfig
