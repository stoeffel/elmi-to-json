module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe
import qualified Data.Aeson as Aeson
import qualified Elm.Json
import Elm.Json (ElmJson(..))
import qualified Elmi
import qualified Info
import Info (Info)
import Subset (Subset(..))

-- TODO error handling, change to ExceptT
run :: IO ()
run = do
  Args {infoFor} <- Args.parse
  ElmJson {elmVersion} <- Elm.Json.load
  modulePaths <-
    case infoFor of
      All -> Elmi.all elmVersion
      Subset modulePaths ->
        return $ Elmi.fromModulePath elmVersion <$> modulePaths
  result <-
    Async.mapConcurrently Info.for modulePaths `withException`
    (\e -> print (e :: SomeAsyncException)) `finally`
    (return ())
  printJSON result

printJSON :: [Info] -> IO ()
printJSON = print . Aeson.encode
