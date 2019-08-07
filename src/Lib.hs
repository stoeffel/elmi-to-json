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
import GHC.Generics (Generic)
import qualified Info
import System.FilePath ((<.>))
import qualified System.FilePath.Extra as FE

run :: IO ()
run = do
  args <- Args.parse
  runUnsafe args `catchAny` onError args

data Result = Result
  { dependencies :: [Info.Dependency]
  , details :: Info.Details
  , internals :: [Info.Internal]
  } deriving (Generic)

instance Aeson.ToJSON Result

runUnsafe :: Args -> IO ()
runUnsafe Args {infoFor, maybeOutput} = do
  elmRoot <- FE.findUp ("elm" <.> "json")
  elmJson <- Elm.Json.load elmRoot
  Elmi.Paths { Elmi.dependencyInterfacePath
             , Elmi.interfacePaths
             , Elmi.detailPaths
             } <- Elmi.for elmRoot elmJson infoFor
  internals <- Async.mapConcurrently Info.forInternal interfacePaths
  dependencies <- Info.forDependencies dependencyInterfacePath
  details <- Info.forDetails detailPaths
  let result = Aeson.encode Result {dependencies, internals, details}
  case maybeOutput of
    Just output -> BL.writeFile output result
    Nothing -> BL.putStr result

onError :: Args -> SomeException -> IO ()
onError Args {infoFor} e =
  mapM_
    putStrLn
    ["elmi-to-json failed" <> " for:", show infoFor, "", show e, ""]
