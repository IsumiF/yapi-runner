{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Backend.Types.Persist where

import           Data.Time
import           Database.Persist.TH
import           GHC.Generics
import           RIO

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Project
  pid Text
  baseUrl Text
  maintainer UserId
  deriving Show Eq Generic
ProjectTestResult
  project ProjectId
  startAt UTCTime
  endAt UTCTime
  resultFiles [Text]
  deriving Show Eq Generic
User
  email Text
  cookie Text
  cookieExpireAt UTCTime
  deriving Show Eq Generic
  |]
