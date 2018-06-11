{-# LANGUAGE DeriveAnyClass #-}

module Info
  ( Info(..)
  , for
  ) where

import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Elm.Interface (Interface)
import qualified Elmi
import GHC.Generics (Generic)
import System.FilePath (FilePath)

data Info = Info
  { moduleName :: T.Text
  , modulePath :: FilePath
  , interface :: Interface
  } deriving (Generic)

instance Aeson.ToJSON Info

for :: Bool -> [FilePath] -> FilePath -> IO (Maybe Info)
for dontFail sourceDirecotries elmiPath = do
  result <- B.decodeFileOrFail elmiPath
  case result of
    Right interface -> do
      maybeModulePath <- Elmi.toModulePath dontFail sourceDirecotries elmiPath
      case maybeModulePath of
        Nothing -> return Nothing
        Just modulePath ->
          return $
          Just $
          Info
          { modulePath = modulePath
          , moduleName = Elmi.toModuleName elmiPath
          , interface = interface
          }
    Left _ -> ES.throwM (DecodingElmiFailed elmiPath)

newtype DecodingElmiFailed =
  DecodingElmiFailed FilePath
  deriving (Typeable, Exception)

instance Show DecodingElmiFailed where
  show (DecodingElmiFailed path) =
    "Couldn't decode " <> path <>
    ". This file seems to be corrupted. Try to nuke `elm-stuff` and `elm make` again."
