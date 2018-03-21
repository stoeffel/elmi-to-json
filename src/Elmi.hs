module Elmi
  ( for
  , toModuleName
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Elm.Json
import Elm.Json (ElmJson(..))
import Prelude hiding (all)
import Subset (Subset(..))
import System.Directory (getDirectoryContents)
import System.FilePath
       (FilePath, (<.>), (</>), dropExtension, makeRelative,
        splitDirectories, takeExtension, takeFileName)

toModuleName :: FilePath -> T.Text
toModuleName = T.replace "-" "." . T.pack . dropExtension . takeFileName

for :: Subset FilePath -> IO [FilePath]
for subset = do
  ElmJson {elmVersion, sourceDirecotries} <- Elm.Json.load
  case subset of
    All -> all elmVersion
    Subset modulePaths ->
      return $ fromModulePath elmVersion sourceDirecotries <$> modulePaths

all :: T.Text -> IO [FilePath]
all version = do
  contents <- getDirectoryContents (elmStuff version)
  let onlyElmi = filter ((==) ".elmi" . takeExtension) contents
  return $ fmap (elmStuff version </>) onlyElmi

fromModulePath :: T.Text -> [FilePath] -> FilePath -> FilePath
fromModulePath version sourceDirecotries modulePath
  -- TODO find elm root (elm.json)
 =
  elmStuff version </>
  T.unpack (dasherize $ removeSourceDir sourceDirecotries modulePath) <.>
  "elmi"

elmStuff :: T.Text -> FilePath
elmStuff version = "elm-stuff" </> T.unpack version

removeSourceDir :: [FilePath] -> FilePath -> FilePath
removeSourceDir dirs dir =
  case L.find (`L.isPrefixOf` dir) dirs of
    Just found -> makeRelative found dir
    Nothing -> dir

dasherize :: FilePath -> T.Text
dasherize = T.intercalate "-" . fmap T.pack . splitDirectories . dropExtension
