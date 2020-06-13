module Common.API.Ping where

import           RIO
import           Servant.API

type API = "ping" :> Get '[JSON] Text
