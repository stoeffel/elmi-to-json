module System.FilePath.Extra
  ( findAll
  , maybeMakeRelative
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import System.Directory (getDirectoryContents)
import System.FilePath
       (FilePath, (<.>), (</>), makeRelative, takeExtension)

findAll :: T.Text -> FilePath -> IO [FilePath]
findAll extension dir = do
  contents <- getDirectoryContents dir
  let found = filter ((==) ("" <.> T.unpack extension) . takeExtension) contents
  return $ fmap (dir </>) found

maybeMakeRelative :: FilePath -> FilePath -> Maybe FilePath
maybeMakeRelative file dir =
  if file `L.isPrefixOf` dir
    then Just (makeRelative dir file)
    else Nothing
