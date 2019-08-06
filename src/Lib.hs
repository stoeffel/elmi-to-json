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
import Info (Info)
import System.FilePath ((<.>))
import qualified System.FilePath.Extra as FE

run :: IO ()
run = do
  args <- Args.parse
  runUnsafe args `catchAny` onError args

data Result = Result
  { dependencyInterfaces :: [Info]
  , interfaces :: [Info]
  } deriving (Generic)

instance Aeson.ToJSON Result

runUnsafe :: Args -> IO ()
runUnsafe Args {infoFor, maybeOutput} = do
  elmRoot <- FE.findUp ("elm" <.> "json")
  elmJson <- Elm.Json.load elmRoot
  Elmi.Paths {Elmi.dependencyInterface, Elmi.interfaces} <-
    Elmi.for elmRoot elmJson infoFor
  interfaces' <- mconcat <$> Async.mapConcurrently Info.for interfaces
  dependencyInterfaces <- Info.forDependencyInterface dependencyInterface
  let result =
        Aeson.encode Result {dependencyInterfaces, interfaces = interfaces'}
  case maybeOutput of
    Just output -> BL.writeFile output result
    Nothing -> BL.putStr result

onError :: Args -> SomeException -> IO ()
onError Args {infoFor} e =
  mapM_
    putStrLn
    ["elmi-to-json failed" <> " for:", show infoFor, "", show e, ""]
