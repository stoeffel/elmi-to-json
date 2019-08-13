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
import Subset (Subset)
import System.FilePath ((<.>))
import qualified System.FilePath.Extra as FE
import qualified UnliftIO

main :: IO ()
main = do
  Args.Args {Args.infoFor, Args.maybeOutput} <- Args.parse
  result <- Task.run (run infoFor)
  case result of
    Right val -> do
      let json = Aeson.encode val
      case maybeOutput of
        Just output -> BL.writeFile output json
        Nothing -> BL.putStr json
    Left err -> do
      onError infoFor err

data Result = Result
  { dependencies :: [Info.Dependency]
  , details :: Info.Details
  , internals :: [Info.Internal]
  } deriving (Generic)

instance Aeson.ToJSON Result

run :: Subset FilePath -> Task Error Result
run infoFor = do
  elmRoot <- Task.io $ FE.findUp ("elm" <.> "json")
  elmJson <- Elm.Json.load elmRoot
  Elmi.Paths { Elmi.dependencyInterfacePath
             , Elmi.interfacePaths
             , Elmi.detailPaths
             } <- Elmi.for elmRoot elmJson infoFor
  internals <- mapConcurrently Info.forInternal interfacePaths
  dependencies <- Info.forDependencies dependencyInterfacePath
  details <- Info.forDetails detailPaths
  return Result {dependencies, internals, details}

mapConcurrently ::
     (Traversable t, UnliftIO.Exception err)
  => (a -> Task err b)
  -> t a
  -> Task err (t b)
mapConcurrently f =
  Task.eio id .
  UnliftIO.try . UnliftIO.mapConcurrently (UnliftIO.fromEitherM . Task.run . f)

onError :: Subset FilePath -> Error -> IO ()
onError infoFor e =
  putStrLn $
  unlines ["elmi-to-json failed" <> " for:", show infoFor, "", show e, ""]
