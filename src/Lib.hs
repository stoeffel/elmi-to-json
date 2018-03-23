module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (SomeAsyncException, withException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Elmi
import qualified Info

run :: IO ()
run = runUnsafe `withException` (print :: SomeAsyncException -> IO ())

runUnsafe :: IO ()
runUnsafe = do
  Args {infoFor, maybeOutput} <- Args.parse
  modulePaths <- Elmi.for infoFor
  result <- Aeson.encode <$> Async.mapConcurrently Info.for modulePaths
  case maybeOutput of
    Just output -> BL.writeFile output result
    Nothing -> print result
