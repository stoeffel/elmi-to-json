module Elmi
  ( for
  , toModuleName
  , InterfacePaths(..)
  , Paths(..)
  ) where

import Control.Applicative ((<|>))
import Data.Foldable (find)
import Data.Maybe (catMaybes)
import qualified Data.Maybe as M
import qualified Data.Text as T
import Elm.Json (ElmJson(..))
import qualified Error
import Error (Error)
import qualified Reporting.Task as Task
import Reporting.Task (Task)
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

for :: FilePath -> ElmJson -> Subset FilePath -> Task Error Paths
for elmRoot elmJson@ElmJson {sourceDirecotries} subset = do
  elmStuffPath <- findElmStuffPath elmRoot elmJson
  let dependencyInterfacePath = elmStuffPath </> "i" <.> "dat"
  let detailPaths = elmStuffPath </> "d" <.> "dat"
  interfacePaths <-
    Task.io $
    catMaybes <$>
    case subset of
      All -> do
        files <- FE.findAll ".elmi" elmStuffPath
        traverse (withModulePath sourceDirecotries) files
      Subset modulePaths -> do
        traverse
          (withElmiPath elmRoot elmStuffPath sourceDirecotries)
          modulePaths
  return Paths {detailPaths, dependencyInterfacePath, interfacePaths}

findElmStuffPath :: FilePath -> ElmJson -> Task Error FilePath
findElmStuffPath elmRoot ElmJson {elmVersion} = do
  let elmStuff = elmRoot </> "elm-stuff"
  elmStuffDirs <- Task.io $ Dir.getDirectoryContents (elmStuff)
  let foundExact = find ((==) elmVersion . T.pack) elmStuffDirs
  let foundPrefix = find (T.isPrefixOf elmVersion . T.pack) elmStuffDirs
  case foundExact <|> foundPrefix of
    Just found -> return (elmStuff </> found)
    Nothing -> Task.throw (Error.ElmStuffNotFound elmVersion)

withModulePath :: [FilePath] -> FilePath -> IO (Maybe InterfacePaths)
withModulePath [] _ = return Nothing
withModulePath (dir:rest) path = do
  exists <- Dir.doesFileExist (dir </> toFileName path)
  if exists
    then return $
         Just
           InterfacePaths
           {interfacePath = path, modulePath = (dir </> toFileName path)}
    else withModulePath rest path
  where
    toFileName =
      flip F.addExtension "elm" .
      T.unpack . T.replace "-" "/" . T.pack . F.dropExtension . F.takeFileName

withElmiPath ::
     FilePath -> FilePath -> [FilePath] -> FilePath -> IO (Maybe InterfacePaths)
withElmiPath elmRoot elmStuffPath sourceDirecotries modulePath = do
  exists <- Dir.doesFileExist modulePath
  if exists
    then do
      relativeToRoot <-
        F.makeRelative elmRoot . F.normalise <$> Dir.makeAbsolute modulePath
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
