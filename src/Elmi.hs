module Elmi
  ( all
  , fromModulePath
  , moduleName
  ) where

import qualified Data.Text as T
import Prelude hiding (all)
import System.Directory (getDirectoryContents)
import System.FilePath
       (FilePath, (<.>), (</>), dropExtension, splitDirectories,
        takeExtension, takeFileName)

all :: IO [FilePath]
all = do
  contents <- getDirectoryContents elmStuff
  let onlyElmi = filter ((==) ".elmi" . takeExtension) contents
  return $ ((</>) elmStuff) <$> onlyElmi

fromModulePath :: FilePath -> FilePath
fromModulePath modulePath
  -- TODO find elm root (elm.json)
  -- TODO remove source-directories
 = elmStuff </> T.unpack (dasherize modulePath) <.> "elmi"

-- TODO check source-dirs
moduleName :: FilePath -> T.Text
moduleName = T.replace "-" "." . T.pack . dropExtension . takeFileName

elmStuff :: FilePath
elmStuff = "elm-stuff" </> "0.19.0"

dasherize :: FilePath -> T.Text
dasherize = T.intercalate "-" . fmap T.pack . splitDirectories . dropExtension
