module Lib
  ( main
  ) where

import qualified Args
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Elm.Json
import qualified Elmi
import Error (Error)
import GHC.Generics (Generic)
import qualified Info
import qualified Reporting.Task as Task
import Reporting.Task (Task)
import qualified Reporting.Task.Async as Async
import System.FilePath ((<.>))
import qualified System.FilePath.Extra as FE

main :: IO ()
main = do
  Args.Args {Args.infoFor, Args.maybeOutput} <- Args.parse
  result <- Task.run (run infoFor)
  case result of
    Left err -> onError infoFor err
    Right val -> do
      let json = Aeson.encode val
      case maybeOutput of
        Just output -> BL.writeFile output json
        Nothing -> BL.putStr json

data Result = Result
  { dependencies :: [Info.Dependency]
  , details :: Info.Details
  , internals :: [Info.Internal]
  } deriving (Generic)

instance Aeson.ToJSON Result

run :: [FilePath] -> Task Error Result
run infoFor = do
  elmRoot <- Task.io $ FE.findUp ("elm" <.> "json")
  elmJson <- Elm.Json.load elmRoot
  Elmi.Paths { Elmi.dependencyInterfacePath
             , Elmi.interfacePaths
             , Elmi.detailPaths
             } <- Elmi.for elmRoot elmJson infoFor
  dependencies <- Info.forDependencies dependencyInterfacePath
  internals <- Async.mapConcurrently Info.forInternal interfacePaths
  details <- Info.forDetails detailPaths
  return Result {dependencies, internals, details}

onError :: [FilePath] -> Error -> IO ()
onError infoFor e =
  putStrLn $
  unlines ["elmi-to-json failed" <> " for:", show infoFor, "", show e, ""]
