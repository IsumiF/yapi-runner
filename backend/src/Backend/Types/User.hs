{-# LANGUAGE TemplateHaskell #-}

module Backend.Types.User
  ( Session(..)
  , session_email
  , User(..)
  , UserId
  , EntityField(UserEmail, UserCookie, UserCookieExpireAt)
  ) where

import           Backend.Types.Persist
import           Backend.Util          (aesonOptions)
import           Control.Lens
import           Data.Aeson
import           GHC.Generics
import           RIO

newtype Session = Session
  { _session_email :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Session where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Session where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

makeLenses ''Session
