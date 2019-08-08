module Lib
  ( run
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
import Subset (Subset)
import System.FilePath ((<.>))
import qualified System.FilePath.Extra as FE

run :: IO ()
run = do
  Args.Args {Args.infoFor, Args.maybeOutput} <- Args.parse
  result <- Task.run (task infoFor)
  case result of
    Right val -> do
      let json = Aeson.encode val
      case maybeOutput of
        Just output -> BL.writeFile output json
        Nothing -> BL.putStr json
    Left err -> onError infoFor err

data Result = Result
  { dependencies :: [Info.Dependency]
  , details :: Info.Details
  , internals :: [Info.Internal]
  } deriving (Generic)

instance Aeson.ToJSON Result

task :: Subset FilePath -> Task Error Result
task infoFor = do
  elmRoot <- Task.io $ FE.findUp ("elm" <.> "json")
  elmJson <- Elm.Json.load elmRoot
  Elmi.Paths { Elmi.dependencyInterfacePath
             , Elmi.interfacePaths
             , Elmi.detailPaths
             } <- Elmi.for elmRoot elmJson infoFor
  internals <- Task.mapConcurrently Info.forInternal interfacePaths
  dependencies <- Info.forDependencies dependencyInterfacePath
  details <- Info.forDetails detailPaths
  return Result {dependencies, internals, details}

onError :: Subset FilePath -> Error -> IO ()
onError infoFor e =
  putStrLn $
  unlines ["elmi-to-json failed" <> " for:", show infoFor, "", show e, ""]
