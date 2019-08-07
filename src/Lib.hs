module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))
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
  Args {infoFor, maybeOutput} <- Args.parse
  result <- Task.run (task infoFor)
  case result of
    Right val ->
      case maybeOutput of
        Just output -> BL.writeFile output val
        Nothing -> BL.putStr val
    Left err -> onError infoFor err

data Result = Result
  { dependencies :: [Info.Dependency]
  , details :: Info.Details
  , internals :: [Info.Internal]
  } deriving (Generic)

instance Aeson.ToJSON Result

task :: Subset FilePath -> Task Error BL.ByteString
task infoFor = do
  elmRoot <- Task.io $ FE.findUp ("elm" <.> "json")
  elmJson <- Elm.Json.load elmRoot
  Elmi.Paths { Elmi.dependencyInterfacePath
             , Elmi.interfacePaths
             , Elmi.detailPaths
             } <- Elmi.for elmRoot elmJson infoFor
  internals <- traverse Info.forInternal interfacePaths
  dependencies <- Info.forDependencies dependencyInterfacePath
  details <- Info.forDetails detailPaths
  return $ Aeson.encode Result {dependencies, internals, details}

onError :: Subset FilePath -> Error -> IO ()
onError infoFor e =
  putStrLn $
  unlines ["elmi-to-json failed" <> " for:", show infoFor, "", show e, ""]
