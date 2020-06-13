module Backend.Handler
  ( mkWaiApplication
  ) where

import           Backend.App
import           Backend.Types.User
import           Common.API                       (API)
import           Control.Monad.Except
import           Network.Wai                      (Request)
import           RIO                              hiding (Handler)
import           Servant.API
import           Servant.Server
import           Servant.Server.Experimental.Auth

import qualified Backend.Handler.Auth             as Auth
import qualified Backend.Handler.Ping             as Ping

mkWaiApplication :: App -> Application
mkWaiApplication app = serveWithContext apiProxy (handlerContext app) $
  hoistServerWithContext apiProxy (Proxy :: Proxy ContextType) (rioToHandler app) handler

rioToHandler :: App -> ExceptT ServerError (RIO App) a -> Handler a
rioToHandler app = Handler . ExceptT . runRIO app . runExceptT

type ContextType = AuthHandler Request Session ': '[]

handlerContext :: App -> Context ContextType
handlerContext app = authHandler :. EmptyContext
  where
    authHandler = AuthHandler $ \request -> (rioToHandler app) (Auth.checkAuth request)

apiProxy :: Proxy API
apiProxy = Proxy

handler :: ServerT API (ExceptT ServerError (RIO App))
handler = (Auth.handleLogin :<|> Auth.handleLoginCallback)
  :<|> Ping.handlePing
