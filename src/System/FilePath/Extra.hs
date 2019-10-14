module System.FilePath.Extra
  ( findAll,
    maybeMakeRelative,
    dasherize,
    findUp,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Error (Error)
import qualified Error
import qualified Reporting.Task as Task
import Reporting.Task (Task)
import qualified System.Directory as Dir
import System.FilePath ((</>), FilePath)
import qualified System.FilePath as F

findAll :: Maybe T.Text -> FilePath -> Task Error [FilePath]
findAll maybeExtension dir = do
  exists <- Task.io (Dir.doesDirectoryExist dir)
  if exists
    then do
      contents <- Task.io (Dir.getDirectoryContents dir)
      case maybeExtension of
        Nothing -> return contents
        Just extension ->
          return $
            F.combine dir
              <$> filter ((==) (T.unpack extension) . F.takeExtension) contents
    else Task.throw (Error.DirectoryDoesntExist dir)

findUp :: FilePath -> IO (Maybe FilePath)
findUp needle = do
  cwd <- Dir.getCurrentDirectory
  findUpHelp needle cwd

findUpHelp :: FilePath -> FilePath -> IO (Maybe FilePath)
findUpHelp needle dir = do
  exists <- Dir.doesFileExist (dir </> needle)
  if exists
    then return (Just dir)
    else case parent dir of
      Just p -> findUpHelp needle p
      Nothing -> pure Nothing

parent :: FilePath -> Maybe FilePath
parent path =
  case F.splitDirectories path of
    [] -> Nothing
    _ : [] -> Nothing
    _ : rest -> Just (F.joinPath rest)

maybeMakeRelative :: FilePath -> FilePath -> Maybe FilePath
maybeMakeRelative file dir =
  if dir `L.isPrefixOf` file
    then Just (F.makeRelative dir file)
    else Nothing

dasherize :: FilePath -> FilePath
dasherize = L.intercalate "-" . F.splitDirectories . F.dropExtension
