module Elmi
  ( all
  , fromModulePath
  , toModuleName
  ) where

import qualified Data.Text as T
import Prelude hiding (all)
import System.Directory (getDirectoryContents)
import System.FilePath
       (FilePath, (<.>), (</>), dropExtension, splitDirectories,
        takeExtension, takeFileName)

all :: T.Text -> IO [FilePath]
all version = do
  contents <- getDirectoryContents (elmStuff version)
  let onlyElmi = filter ((==) ".elmi" . takeExtension) contents
  return $ fmap (elmStuff version </>) onlyElmi

fromModulePath :: T.Text -> FilePath -> FilePath
fromModulePath version modulePath
  -- TODO find elm root (elm.json)
  -- TODO remove source-directories
 = elmStuff version </> T.unpack (dasherize modulePath) <.> "elmi"

-- TODO check source-dirs
toModuleName :: FilePath -> T.Text
toModuleName = T.replace "-" "." . T.pack . dropExtension . takeFileName

elmStuff :: T.Text -> FilePath
elmStuff version = "elm-stuff" </> T.unpack version

dasherize :: FilePath -> T.Text
dasherize = T.intercalate "-" . fmap T.pack . splitDirectories . dropExtension
