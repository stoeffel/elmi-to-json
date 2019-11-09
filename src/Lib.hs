module Lib
  ( main,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Version (showVersion)
import qualified Elm.Json
import qualified Elmi
import Error (Error)
import qualified Error
import qualified ForElmTest
import qualified Info
import qualified Options
import Options (Options (..))
import Paths_elmi_to_json (version)
import qualified Reporting.Task as Task
import Reporting.Task (Task)
import qualified Reporting.Task.Async as Async
import qualified Result
import Result (Result (Result))
import System.FilePath ((<.>))
import qualified System.FilePath.Extra as FE

main :: IO ()
main = do
  mode <- Options.parse
  case mode of
    Options.Version -> putStrLn (showVersion version)
    Options.Run runMode Options {files, output, elmVersion} -> do
      result <- Task.run (run elmVersion files)
      case result of
        Left err -> onError files err
        Right val -> printOutput output $
          case runMode of
            Options.ForElmTest -> Aeson.encode (ForElmTest.fromResult val)
            Options.Normal -> Aeson.encode val

printOutput :: Options.Output -> BL.ByteString -> IO ()
printOutput output content =
  case output of
    Options.OutputFile output' -> BL.writeFile output' content
    Options.OutputStdout -> BL.putStr content

run :: Options.ElmVersion -> [FilePath] -> Task Error Result
run elmVersion files = do
  maybeElmRoot <- Task.io $ FE.findUp ("elm" <.> "json")
  elmRoot <-
    case maybeElmRoot of
      Just root -> pure root
      Nothing -> Task.throw Error.NoElmJson
  elmJson <- Elm.Json.load elmRoot
  Elmi.Paths
    { Elmi.dependencyInterfacePath,
      Elmi.interfacePaths,
      Elmi.detailPaths
    } <-
    Elmi.for elmRoot elmVersion elmJson files
  dependencies <- Info.forDependencies dependencyInterfacePath
  internals <- Async.mapConcurrently Info.forInternal interfacePaths
  details <- Info.forDetails detailPaths
  return Result {Result.dependencies, Result.internals, Result.details}

onError :: [FilePath] -> Error -> IO ()
onError files e =
  putStrLn
    $ unlines
    $ mconcat
      [ ["elmi-to-json failed", "===================", ""],
        case files of
          [] -> []
          _ -> "Ran for:" : files <> [""],
        [show e, ""]
      ]
