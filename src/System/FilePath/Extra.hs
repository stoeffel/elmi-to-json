module System.FilePath.Extra
  ( findAll
  , maybeMakeRelative
  , dasherize
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath (FilePath, (<.>))
import qualified System.FilePath as F

findAll :: T.Text -> FilePath -> IO [FilePath]
findAll extension dir = do
  contents <- Dir.getDirectoryContents dir
  return $
    F.combine dir <$>
    filter ((==) ("" <.> T.unpack extension) . F.takeExtension) contents

maybeMakeRelative :: FilePath -> FilePath -> Maybe FilePath
maybeMakeRelative file dir =
  if dir `L.isPrefixOf` file
    then Just (F.makeRelative dir file)
    else Nothing

dasherize :: FilePath -> FilePath
dasherize = L.intercalate "-" . F.splitDirectories . F.dropExtension
