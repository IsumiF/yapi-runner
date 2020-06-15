module Common.API.Auth
  ( API
  ) where

import           RIO
import           Servant.API

type API = "auth" :>
  ( APILogin
  :<|> APILoginCallback
  )

type APILogin = "login"
  :> QueryParam "next" Text
  :> Verb 'GET 302 '[PlainText] (Headers '[Header "Location" Text] NoContent)

type APILoginCallback = "login"
  :> "callback"
  :> QueryParam "state" Text
  :> QueryParam "code" Text
  :> QueryParam "error" Text
  :> Verb 'GET 302 '[PlainText] (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)
