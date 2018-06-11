{-# LANGUAGE DeriveAnyClass #-}

module Elmi
  ( for
  , toModuleName
  , Paths(..)
  ) where

import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Data.Maybe (catMaybes)
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

data Paths = Paths
  { interfacePath :: FilePath
  , modulePath :: FilePath
  }

for :: FilePath -> ElmJson -> Subset FilePath -> IO [Paths]
for elmRoot elmJson@ElmJson {elmVersion, sourceDirecotries} subset =
  case subset of
    All -> do
      files <- FE.findAll ".elmi" (elmRoot </> elmStuff elmVersion)
      filesWithModulePath <-
        catMaybes <$> traverse (withModulePath sourceDirecotries) files
      return filesWithModulePath
    Subset modulePaths -> do
      files <- traverse (withElmiPath elmRoot elmJson) modulePaths
      return files

withModulePath :: [FilePath] -> FilePath -> IO (Maybe Paths)
withModulePath sourceDirecotries path =
  case sourceDirecotries of
    dir:rest -> do
      exists <- Dir.doesFileExist (dir </> toFileName path)
      if exists
        then return $
             Just
               Paths
               {interfacePath = path, modulePath = (dir </> toFileName path)}
        else withModulePath rest path
    [] -> return Nothing
  where
    toFileName =
      flip F.addExtension "elm" .
      T.unpack . T.replace "-" "/" . T.pack . F.dropExtension . F.takeFileName

withElmiPath :: FilePath -> ElmJson -> FilePath -> IO Paths
withElmiPath elmRoot ElmJson {elmVersion, sourceDirecotries} modulePath = do
  exists <- Dir.doesFileExist (modulePath)
  if not exists
    then ES.throwM (ModuleNotFound modulePath)
    else do
      relativeToRoot <-
        (F.makeRelative elmRoot . F.normalise) <$> Dir.makeAbsolute modulePath
      let elmiName =
            FE.dasherize $ removeSourceDir relativeToRoot sourceDirecotries
      return
        Paths
        { interfacePath =
            (elmRoot </> elmStuff elmVersion </> elmiName <.> "elmi")
        , modulePath = modulePath
        }

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
