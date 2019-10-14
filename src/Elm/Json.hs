module Elm.Json
  ( ElmJson(..)
  , ElmVersion(..)
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
  , elmVersion :: ElmVersion
  }


instance Aeson.FromJSON ElmJson where
  parseJSON =
    Aeson.withObject "Coord" $ \v ->
      ElmJson <$> v .:? "source-directories" .!= ["src", "tests"] <*>
      (parseVersion <$> v .: "elm-version")


data ElmVersion
  = FixedVersion T.Text
  | RangedVersion

load :: FilePath -> Task Error ElmJson
load root = do
  let elmJsonPath = (root </> "elm" <.> "json")
  contents <- Task.io $ BL.readFile elmJsonPath
  case Aeson.eitherDecode contents of
    Right json -> return json
    Left _ -> Task.throw (Error.DecodingElmJsonFailed elmJsonPath)


parseVersion :: T.Text -> ElmVersion
parseVersion str =
  case T.words str of
    [fixed] -> FixedVersion fixed
    -- 0.19.0 <= v <= 0.20.0
    _ -> RangedVersion
