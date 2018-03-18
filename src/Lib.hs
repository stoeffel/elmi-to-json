module Lib
  ( run
  ) where

import Args (Args(..), Subset(..))
import qualified Args
import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Elm.Interface (Interface)
import System.Directory (getDirectoryContents)
import System.FilePath
       (FilePath, (<.>), (</>), dropExtension, splitDirectories,
        takeExtension)

run :: IO ()
run = do
  Args {infoFor} <- Args.parse
  modulePaths <- infoForWhatSubset infoFor
  result <- traverse (B.decodeFileOrFail . modulePathToElmi) modulePaths
  case partitionEithers result of
    ([], decoded) -> printJSON decoded
    (errs, []) -> print errs -- TODO exitcode
    _ -> putStrLn "failed" -- TODO exitcode

printJSON :: [Interface] -> IO ()
printJSON = print . Aeson.encode

infoForWhatSubset :: Subset FilePath -> IO [FilePath]
infoForWhatSubset subset =
  case subset of
    Subset modulePaths -> return modulePaths
    All ->
      filter ((==) ".elmi" . takeExtension) <$>
      getDirectoryContents elmStuffPath

modulePathToElmi :: FilePath -> FilePath
modulePathToElmi modulePath
  -- TODO find elm root (elm.json)
 = elmStuffPath </> T.unpack (dasherizePath modulePath) <.> "elmi"

elmStuffPath :: FilePath
elmStuffPath = "elm-stuff" </> "0.19.0"

dasherizePath :: FilePath -> T.Text
dasherizePath =
  T.intercalate "-" . fmap T.pack . splitDirectories . dropExtension
