module Elm.Json
  ( ElmJson(..)
  , load
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Error
import Error (Error)
import qualified Reporting.Task as Task
import Reporting.Task (Task)
import System.FilePath (FilePath, (<.>), (</>))

data ElmJson = ElmJson
  { sourceDirecotries :: [FilePath]
  , elmVersion :: T.Text
  }

instance Aeson.FromJSON ElmJson where
  parseJSON =
    Aeson.withObject "Coord" $ \v ->
      ElmJson <$> v .:? "source-directories" .!= ["src", "tests"] <*>
      (T.takeWhile ((/=) ' ') <$> v .: "elm-version")

load :: FilePath -> Task Error ElmJson
load root = do
  let elmJsonPath = (root </> "elm" <.> "json")
  contents <- Task.io $ BL.readFile elmJsonPath
  case Aeson.eitherDecode contents of
    Right json -> return json
    Left _ -> Task.throw (Error.DecodingElmJsonFailed elmJsonPath)
