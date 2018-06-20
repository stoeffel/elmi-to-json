{-# LANGUAGE DeriveAnyClass #-}

module Elm.Json
  ( ElmJson(..)
  , load
  ) where

import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import qualified Data.Aeson as Aeson
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
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

load :: FilePath -> IO ElmJson
load root = do
  let elmJsonPath = (root </> "elm" <.> "json")
  contents <- BL.readFile elmJsonPath
  case Aeson.eitherDecode contents of
    Right json -> return json
    Left _ -> ES.throwM (DecodingElmJsonFailed elmJsonPath)

newtype DecodingElmJsonFailed =
  DecodingElmJsonFailed FilePath
  deriving (Typeable, Exception)

instance Show DecodingElmJsonFailed where
  show (DecodingElmJsonFailed path) = "Couldn't decode " <> path
