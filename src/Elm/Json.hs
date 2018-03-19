module Elm.Json
  ( ElmJson(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath (FilePath)

data ElmJson = ElmJson
  { sourceDirecotries :: [FilePath]
  , elmVersion :: T.Text
  } deriving (Generic)

instance Aeson.FromJSON ElmJson
