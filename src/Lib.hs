module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args

import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (SomeException, catchAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))
import qualified Elmi
import qualified Info
import qualified Rainbow as R

run :: IO ()
run = do
  args <- Args.parse
  runUnsafe args `catchAny` onError args

runUnsafe :: Args -> IO ()
runUnsafe Args {infoFor, maybeOutput} = do
  modulePaths <- Elmi.for infoFor
  result <- Aeson.encode <$> Async.mapConcurrently Info.for modulePaths
  case maybeOutput of
    Just output -> BL.writeFile output result
    Nothing -> print result

onError :: Args -> SomeException -> IO ()
onError Args {infoFor} e =
  mapM_
    (R.putChunkLn . R.fore R.red . R.chunk)
    ["elmi-to-json failed" <> " for:", show infoFor, "", show e, ""]
