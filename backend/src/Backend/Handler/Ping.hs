module Backend.Handler.Ping
  ( handlePing
  ) where

import           Backend.Types.User
import           Control.Monad.Except
import           RIO
import           Servant.Server

handlePing :: Monad m => Session -> ExceptT ServerError m Text
handlePing (Session email) = pure $ "hi, " <> email
