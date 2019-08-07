{-# LANGUAGE DeriveAnyClass #-}

module Elmi
  ( for
  , toModuleName
  , InterfacePaths(..)
  , Paths(..)
  ) where

import Control.Applicative ((<|>))
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
  { interfacePaths :: [InterfacePaths]
  , dependencyInterfacePath :: FilePath
  , detailPaths :: FilePath
  }

data InterfacePaths = InterfacePaths
  { interfacePath :: FilePath
  , modulePath :: FilePath
  }

for :: FilePath -> ElmJson -> Subset FilePath -> IO Paths
for elmRoot elmJson@ElmJson {sourceDirecotries} subset = do
  elmStuffPath <- findElmStuffPath elmRoot elmJson
  interfacePaths <-
    case subset of
      All -> do
        files <- FE.findAll ".elmi" elmStuffPath
        catMaybes <$> traverse (withModulePath sourceDirecotries) files
      Subset modulePaths -> do
        catMaybes <$>
          traverse
            (withElmiPath elmRoot elmStuffPath sourceDirecotries)
            modulePaths
  let dependencyInterfacePath = elmStuffPath </> "i" <.> "dat"
  let detailPaths = elmStuffPath </> "d" <.> "dat"
  return Paths {detailPaths, dependencyInterfacePath, interfacePaths}

findElmStuffPath :: FilePath -> ElmJson -> IO FilePath
findElmStuffPath elmRoot ElmJson {elmVersion} = do
  let elmStuff = "elm-stuff"
  elmStuffDirs <- Dir.getDirectoryContents (elmRoot </> elmStuff)
  let foundExact =
        safeHead . filter (\path -> path == elmVersion) $
        fmap T.pack elmStuffDirs
  let foundPrefix =
        safeHead . filter (\path -> elmVersion `T.isPrefixOf` path) $
        fmap T.pack elmStuffDirs
  case foundExact <|> foundPrefix of
    Just found -> return $ elmRoot </> elmStuff </> (T.unpack found)
    Nothing -> ES.throwM (ElmStuffNotFound elmVersion)

safeHead :: [e] -> Maybe e
safeHead [] = Nothing
safeHead (x:_) = Just x

newtype ElmStuffNotFound =
  ElmStuffNotFound T.Text
  deriving (Typeable, Exception)

instance Show ElmStuffNotFound where
  show (ElmStuffNotFound version) =
    "Couldn't find elm-stuff for Elm version " <> T.unpack version <> "."

withModulePath :: [FilePath] -> FilePath -> IO (Maybe InterfacePaths)
withModulePath sourceDirecotries path =
  case sourceDirecotries of
    dir:rest -> do
      exists <- Dir.doesFileExist (dir </> toFileName path)
      if exists
        then return $
             Just
               InterfacePaths
               {interfacePath = path, modulePath = (dir </> toFileName path)}
        else withModulePath rest path
    [] -> return Nothing
  where
    toFileName =
      flip F.addExtension "elm" .
      T.unpack . T.replace "-" "/" . T.pack . F.dropExtension . F.takeFileName

withElmiPath ::
     FilePath -> FilePath -> [FilePath] -> FilePath -> IO (Maybe InterfacePaths)
withElmiPath elmRoot elmStuffPath sourceDirecotries modulePath = do
  exists <- Dir.doesFileExist (modulePath)
  if exists
    then do
      relativeToRoot <-
        (F.makeRelative elmRoot . F.normalise) <$> Dir.makeAbsolute modulePath
      let elmiName =
            FE.dasherize $ removeSourceDir relativeToRoot sourceDirecotries
      return $
        Just
          InterfacePaths
          { interfacePath = (elmStuffPath </> elmiName <.> "elmi")
          , modulePath = modulePath
          }
    else return Nothing

removeSourceDir :: FilePath -> [FilePath] -> FilePath
removeSourceDir file dirs =
  case M.mapMaybe (FE.maybeMakeRelative file) dirs of
    (x:_) -> x
    [] -> file
