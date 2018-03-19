module Info
  ( Info(..)
  , for
  ) where

import Control.Exception.Safe
import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import Elm.Interface (Interface)
import qualified Elmi
import GHC.Generics (Generic)

data Info = Info
  { moduleName :: T.Text
  , interface :: Interface
  } deriving (Generic)

instance Aeson.ToJSON Info

for :: FilePath -> IO Info
for modulePath = do
  result <- B.decodeFileOrFail modulePath
  case result of
    Right interface ->
      return
        Info {moduleName = Elmi.toModuleName modulePath, interface = interface}
    Left _ -> throwM (DecodingElmiFailed modulePath)

newtype DecodingElmiFailed =
  DecodingElmiFailed FilePath
  deriving (Typeable)

instance Show DecodingElmiFailed where
  show (DecodingElmiFailed path) =
    "Couldn't decode " <> path <>
    ". This file seems to be corrupted. Try to nuke `elm-stuff` and `elm make` again."

instance Exception DecodingElmiFailed where
  toException = toException . SomeAsyncException
  fromException se = do
    SomeAsyncException e <- fromException se
    cast e
