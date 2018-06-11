{-# LANGUAGE DeriveAnyClass #-}

module Elmi
  ( for
  , toModuleName
  , toModulePath
  ) where

import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import qualified Data.Maybe as M
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Elm.Json (ElmJson(..))
import Subset (Subset(..))
import qualified System.Directory as Dir
import System.FilePath (FilePath, (<.>), (</>))
import qualified System.FilePath as F
import qualified System.FilePath.Extra as FE

toModuleName :: FilePath -> T.Text
toModuleName = T.replace "-" "." . T.pack . F.dropExtension . F.takeFileName

toModulePath :: Bool -> [FilePath] -> FilePath -> IO (Maybe FilePath)
toModulePath dontFail sourceDirecotries path =
  case sourceDirecotries of
    dir:rest -> do
      exists <- Dir.doesFileExist (dir </> toFileName path)
      if exists
        then return $ Just (dir </> toFileName path)
        else toModulePath dontFail rest path
    [] ->
      if dontFail
        then return Nothing
        else ES.throwM (ModuleNotFound path)
  where
    toFileName =
      flip F.addExtension "elm" .
      T.unpack . T.replace "-" "/" . T.pack . F.dropExtension . F.takeFileName

for :: FilePath -> ElmJson -> Subset FilePath -> IO (Bool, [FilePath])
for elmRoot elmJson@ElmJson {elmVersion} subset =
  case subset of
    All -> do
      files <- FE.findAll ".elmi" (elmRoot </> elmStuff elmVersion)
      return (True, files)
    Subset modulePaths -> do
      files <- traverse (toElmiPath elmRoot elmJson) modulePaths
      return (False, files)

toElmiPath :: FilePath -> ElmJson -> FilePath -> IO FilePath
toElmiPath elmRoot ElmJson {elmVersion, sourceDirecotries} modulePath = do
  relativeToRoot <-
    (F.makeRelative elmRoot . F.normalise) <$> Dir.makeAbsolute modulePath
  let elmiName = FE.dasherize $ removeSourceDir relativeToRoot sourceDirecotries
  return (elmRoot </> elmStuff elmVersion </> elmiName <.> "elmi")

elmStuff :: T.Text -> FilePath
elmStuff version = "elm-stuff" </> T.unpack version

removeSourceDir :: FilePath -> [FilePath] -> FilePath
removeSourceDir file dirs =
  case M.mapMaybe (FE.maybeMakeRelative file) dirs of
    (x:_) -> x
    [] -> file

newtype ModuleNotFound =
  ModuleNotFound FilePath
  deriving (Typeable, Exception)

instance Show ModuleNotFound where
  show (ModuleNotFound path) =
    "I didn't find a module for the interface " <> path <> "."
