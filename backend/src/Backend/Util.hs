{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Backend.Util
  ( plus2
  , aesonOptions
  ) where

import           Data.Aeson
import           RIO
import qualified RIO.List.Partial as L'

plus2 :: Int -> Int
plus2 = (+ 2)

aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = L'.tail . dropWhile (/= '_') . L'.tail
  }
