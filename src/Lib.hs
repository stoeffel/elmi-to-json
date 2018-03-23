module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args

import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (SomeException, catchAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Elmi
import qualified Info
import qualified Rainbow as R

run :: IO ()
run = catchAny runUnsafe onError

runUnsafe :: IO ()
runUnsafe = do
  Args {infoFor, maybeOutput} <- Args.parse
  modulePaths <- Elmi.for infoFor
  result <- Aeson.encode <$> Async.mapConcurrently Info.for modulePaths
  case maybeOutput of
    Just output -> BL.writeFile output result
    Nothing -> print result

onError :: SomeException -> IO ()
onError = R.putChunkLn . R.fore R.red . R.chunk . show
