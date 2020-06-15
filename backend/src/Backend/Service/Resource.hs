module Backend.Service.Resource where

import           RIO
import qualified RIO.ByteString as BS
import           RIO.FilePath
import           RIO.Process
import qualified RIO.Text       as T

class MonadResource m where
  readResourceBS :: FilePath -> m ByteString

instance HasProcessContext env => MonadResource (RIO env) where
  readResourceBS fp = do
    resourceDir <- fromMaybe "resource" <$> lookupEnvFromContext "RESOURCE_DIR"
    BS.readFile $ T.unpack resourceDir </> fp

