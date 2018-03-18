module Elmi
  ( all
  , fromModulePath
  ) where

import qualified Data.Text as T
import Prelude hiding (all)
import System.Directory (getDirectoryContents)
import System.FilePath
       (FilePath, (<.>), (</>), dropExtension, splitDirectories,
        takeExtension)

all :: IO [FilePath]
all = filter ((==) ".elmi" . takeExtension) <$> getDirectoryContents elmStuff

fromModulePath :: FilePath -> FilePath
fromModulePath modulePath
  -- TODO find elm root (elm.json)
 = elmStuff </> T.unpack (dasherize modulePath) <.> "elmi"

elmStuff :: FilePath
elmStuff = "elm-stuff" </> "0.19.0"

dasherize :: FilePath -> T.Text
dasherize = T.intercalate "-" . fmap T.pack . splitDirectories . dropExtension
