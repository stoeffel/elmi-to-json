module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Elm.Interface (Interface)
import System.FilePath
       (FilePath, (<.>), (</>), dropExtension, splitDirectories)

run :: IO ()
run = do
  Args {modulePaths} <- Args.parse -- TODO map to elmi path
  result <- traverse (B.decodeFileOrFail . modulePathToElmi) modulePaths
  case partitionEithers result of
    ([], decoded) -> printJSON decoded
    (errs, []) -> print errs -- TODO exitcode
    _ -> putStrLn "failed" -- TODO exitcode

printJSON :: [Interface] -> IO ()
printJSON = print . Aeson.encode

modulePathToElmi :: FilePath -> FilePath
modulePathToElmi modulePath
  -- TODO find elm root (elm.json)
 = "elm-stuff" </> "0.19.0" </> (T.unpack $ dasherizePath modulePath) <.> "elmi"

dasherizePath :: FilePath -> T.Text
dasherizePath =
  T.intercalate "-" . fmap T.pack . splitDirectories . dropExtension
