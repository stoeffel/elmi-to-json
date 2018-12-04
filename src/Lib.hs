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
import qualified Elm.Json
import qualified Elmi
import qualified Info
import System.FilePath ((<.>))
import qualified System.FilePath.Extra as FE

run :: IO ()
run = do
  args <- Args.parse
  runUnsafe args `catchAny` onError args

runUnsafe :: Args -> IO ()
runUnsafe Args {infoFor, maybeOutput} = do
  elmRoot <- FE.findUp ("elm" <.> "json")
  elmJson <- Elm.Json.load elmRoot
  modulePaths <- Elmi.for elmRoot elmJson infoFor
  result <- Aeson.encode <$> Async.mapConcurrently Info.for modulePaths
  case maybeOutput of
    Just output -> BL.writeFile output result
    Nothing -> BL.putStr result

onError :: Args -> SomeException -> IO ()
onError Args {infoFor} e =
  mapM_ putStrLn
    ["elmi-to-json failed" <> " for:", show infoFor, "", show e, ""]
