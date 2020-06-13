{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
module Backend.App
  ( withApp
  , App
  ) where

import           Backend.App.Config
import           Backend.Persist    (HasPersist (..), Persist, withPersist)
import           RIO
import           RIO.Process

withApp :: (App -> IO a) -> IO a
withApp action = do
  pc <- mkDefaultProcessContext
  config <- runRIO pc loadConfig
  lo <- logOptionsHandle stdout True
  withLogFunc lo $ \lf ->
    runRIO lf $ withPersist (_config_sqlite config) $ \persist -> do
      let app = App
                { appLogFunc = lf
                , appProcessContext = pc
                , appPersist = persist
                }
      RIO (lift $ action app)

data App = App
  { appLogFunc        :: LogFunc
  , appProcessContext :: ProcessContext
  , appPersist        :: Persist
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasPersist App where
  persistL = lens appPersist (\x y -> x { appPersist = y })
