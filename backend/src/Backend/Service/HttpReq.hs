module Backend.Service.HttpReq
  ( module Network.HTTP.Req
  , MonadHttpReq(..)
  ) where

import           Backend.Import
import           Network.HTTP.Req hiding (runReq)
import qualified Network.HTTP.Req as Req (runReq)

class Monad m => MonadHttpReq m where
  runReq :: HttpConfig -> Req a -> m a

instance MonadHttpReq (RIO env) where
  runReq = Req.runReq
