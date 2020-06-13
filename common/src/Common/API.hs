module Common.API
  ( API
  ) where

import qualified Common.API.Auth as Auth
import qualified Common.API.Ping as Ping
import           Servant.API

type API = "api" :>
  ( Auth.API
  :<|> AuthProtect "cookie-auth" :> Ping.API
  )
