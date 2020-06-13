module Backend.Persist
  ( Persist
  , withPersist
  , HasPersist(..)
  ) where

import           Backend.Persist.Config
import           Data.Pool
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           RIO
import           RIO.Orphans             ()

newtype Persist = Persist
  { _persist_connPool :: Pool SqlBackend
  }

withPersist :: HasLogFunc env => SqliteConfig -> (Persist -> RIO env a) -> RIO env a
withPersist (SqliteConfig file poolSize) action = withSqlitePool file poolSize (action . Persist)

class HasPersist env where
  persistL :: Lens' env Persist
