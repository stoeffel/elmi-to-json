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

for :: FilePath -> IO Info
for modulePath = do
  result <- B.decodeFileOrFail modulePath
  case result of
    Right interface ->
      return
        Info
        { modulePath = Elmi.toModulePath modulePath
        , moduleName = Elmi.toModuleName modulePath
        , interface = interface
        }
    Left _ -> ES.throwM (DecodingElmiFailed modulePath)

newtype DecodingElmiFailed =
  DecodingElmiFailed FilePath
  deriving (Typeable, Exception)

instance Show DecodingElmiFailed where
  show (DecodingElmiFailed path) =
    "Couldn't decode " <> path <>
    ". This file seems to be corrupted. Try to nuke `elm-stuff` and `elm make` again."
