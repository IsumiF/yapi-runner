module Backend.Handler
  ( mkWaiApplication
  ) where

import           Backend.App
import qualified Common.API.Ping          as Ping
import           Control.Monad.Except
import           RIO                      hiding (Handler)
import           Servant.Server

mkWaiApplication :: App -> Application
mkWaiApplication app = serve
  (Proxy :: Proxy Ping.API)
  (hoistServer (Proxy :: Proxy Ping.API) (rioToHandler app) handlePing)

handlePing :: ExceptT ServerError (RIO App) Text
handlePing = pure "pong"

rioToHandler :: App -> ExceptT ServerError (RIO App) a -> Handler a
rioToHandler app = Handler . ExceptT . runRIO app . runExceptT
