module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe as ES
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Elmi
import qualified Info
import Info (Info)

run :: IO ()
run = do
  Args {infoFor, maybeOutput} <- Args.parse
  modulePaths <- Elmi.for infoFor
  result <- Aeson.encode <$> decodeElmi modulePaths
  case maybeOutput of
    Just output -> BL.writeFile output result
    Nothing -> print result

decodeElmi :: [FilePath] -> IO [Info]
decodeElmi modulePaths =
  ES.withException
    (Async.mapConcurrently Info.for modulePaths)
    (print :: ES.SomeAsyncException -> IO ())
