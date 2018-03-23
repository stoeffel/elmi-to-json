module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Elmi
import qualified Info
import Info (Info)

-- TODO error handling, change to ExceptT
run :: IO ()
run = do
  Args {infoFor, maybeOutput} <- Args.parse
  modulePaths <- Elmi.for infoFor
  result <-
    Async.mapConcurrently Info.for modulePaths `withException`
    (\e -> print (e :: SomeAsyncException)) `finally`
    return () :: IO [Info]
  let encoded = Aeson.encode result
  case maybeOutput of
    Just output -> BL.writeFile output encoded
    Nothing -> print encoded
