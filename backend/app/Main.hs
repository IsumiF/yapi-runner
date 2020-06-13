{-# LANGUAGE NoImplicitPrelude #-}
module Main
  ( main
  ) where

import           Backend.App
import           Backend.Handler
import           Network.Wai.Handler.Warp
import           RIO

main :: IO ()
main = withApp $ \app ->
    runEnv 8080 (mkWaiApplication app)
