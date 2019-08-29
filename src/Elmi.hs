module Elmi
  ( for
  , toModuleName
  , InterfacePaths(..)
  , Paths(..)
  ) where

import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Elm.Json (ElmJson(..))
import qualified Error
import Error (Error)
import qualified Reporting.Task as Task
import Reporting.Task (Task)
import qualified System.Directory as Dir
import System.FilePath (FilePath, (<.>), (</>))
import qualified System.FilePath as F
import qualified System.FilePath.Extra as FE

toModuleName :: FilePath -> Text.Text
toModuleName =
  Text.replace "-" "." . Text.pack . F.dropExtension . F.takeFileName

data Paths = Paths
  { interfacePaths :: [InterfacePaths]
  , dependencyInterfacePath :: FilePath
  , detailPaths :: FilePath
  }

data InterfacePaths = InterfacePaths
  { interfacePath :: FilePath
  , modulePath :: FilePath
  }

for :: FilePath -> ElmJson -> [FilePath] -> Task Error Paths
for elmRoot elmJson@ElmJson {sourceDirecotries} subset = do
  elmStuffPath <- findElmStuffPath elmRoot elmJson
  let dependencyInterfacePath = elmStuffPath </> "i" <.> "dat"
  let detailPaths = elmStuffPath </> "d" <.> "dat"
  files <- FE.findAll (Just ".elmi") elmStuffPath
  unfilteredPaths <- Task.io $ traverse (withModulePath sourceDirecotries) files
  let interfacePaths = Maybe.mapMaybe (allOrSubset subset) unfilteredPaths
  return Paths {detailPaths, dependencyInterfacePath, interfacePaths}

findElmStuffPath :: FilePath -> ElmJson -> Task Error FilePath
findElmStuffPath elmRoot ElmJson {elmVersion} = do
  let elmStuff = elmRoot </> "elm-stuff"
  elmStuffDirs <- FE.findAll Nothing elmStuff
  let foundExact = Foldable.find ((==) elmVersion . Text.pack) elmStuffDirs
  let foundPrefix =
        Foldable.find (Text.isPrefixOf elmVersion . Text.pack) elmStuffDirs
  case foundExact <|> foundPrefix of
    Just found -> return (elmStuff </> found)
    Nothing -> Task.throw (Error.ElmStuffNotFound elmVersion)

allOrSubset :: [FilePath] -> Maybe InterfacePaths -> Maybe InterfacePaths
allOrSubset _ Nothing = Nothing
allOrSubset subset (Just (paths@InterfacePaths {modulePath}))
  | subset == [] = Just paths
  | List.elem modulePath subset = Just paths
  | otherwise = Nothing

withModulePath :: [FilePath] -> FilePath -> IO (Maybe InterfacePaths)
withModulePath [] _ = return Nothing
withModulePath (dir:rest) interfacePath = do
  let modulePath = dir </> elmiToModulePath interfacePath
  exists <- Dir.doesFileExist modulePath
  if exists
    then return $ Just InterfacePaths {interfacePath, modulePath}
    else withModulePath rest interfacePath

elmiToModulePath :: FilePath -> FilePath
elmiToModulePath =
  flip F.addExtension "elm" .
  Text.unpack .
  Text.replace "-" "/" . Text.pack . F.dropExtension . F.takeFileName
