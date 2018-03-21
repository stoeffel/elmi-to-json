module Elmi
  ( for
  , toModuleName
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

for :: Subset FilePath -> IO [FilePath]
for subset = do
  ElmJson {elmVersion, sourceDirecotries} <- Elm.Json.load
  case subset of
    All -> FE.findAll ".elmi" (elmStuff elmVersion)
    Subset modulePaths -> do
      cwd <- Dir.getCurrentDirectory
      return
        (toElmiPath elmVersion .
         removeSourceDir sourceDirecotries . F.makeRelative cwd . F.normalise <$>
         modulePaths)

toElmiPath :: T.Text -> FilePath -> FilePath
toElmiPath version modulePath
  -- TODO find elm root (elm.json)
 = elmStuff version </> FE.dasherize modulePath <.> "elmi"

elmStuff :: T.Text -> FilePath
elmStuff version = "elm-stuff" </> T.unpack version

removeSourceDir :: [FilePath] -> FilePath -> FilePath
removeSourceDir dirs file =
  M.fromMaybe file $ M.listToMaybe $ M.mapMaybe (FE.maybeMakeRelative file) dirs
