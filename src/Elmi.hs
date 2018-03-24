module Elmi
  ( for
  , toModuleName
  , toModulePath
  ) where

import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Elm.Json
import Elm.Json (ElmJson(..))
import Subset (Subset(..))
import qualified System.Directory as Dir
import System.FilePath (FilePath, (<.>), (</>))
import qualified System.FilePath as F
import qualified System.FilePath.Extra as FE

toModuleName :: FilePath -> T.Text
toModuleName = T.replace "-" "." . T.pack . F.dropExtension . F.takeFileName

toModulePath :: FilePath -> FilePath
toModulePath
  -- TODO search in source-direcotries
 =
  flip F.addExtension "elm" .
  T.unpack . T.replace "-" "/" . T.pack . F.dropExtension . F.takeFileName

for :: Subset FilePath -> IO [FilePath]
for subset = do
  elmRoot <- FE.findUp ("elm" <.> "json")
  elmJson@ElmJson {elmVersion} <- Elm.Json.load elmRoot
  case subset of
    All -> FE.findAll ".elmi" (elmRoot </> elmStuff elmVersion)
    Subset modulePaths -> traverse (toElmiPath elmRoot elmJson) modulePaths

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
