module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe
import qualified Data.Aeson as Aeson
import qualified Elmi
import qualified Info
import Info (Info)
import Subset (Subset(..))

-- TODO error handling, change to ExceptT
run :: IO ()
run = do
  Args {infoFor} <- Args.parse
  modulePaths <-
    case infoFor of
      All -> Elmi.all
      Subset modulePaths -> return $ Elmi.fromModulePath <$> modulePaths
  result <-
    Async.mapConcurrently Info.for modulePaths `withException`
    (\e -> print (e :: SomeAsyncException)) `finally`
    (return ())
  printJSON result

printJSON :: [Info] -> IO ()
printJSON = print . Aeson.encode
