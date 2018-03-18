module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import Elm.Interface (Interface)
import System.Environment (getArgs)

run :: IO ()
run = do
  Args {modulePaths} <- Args.parse
  result <- traverse B.decodeFileOrFail modulePaths
  case partitionEithers result of
    ([], decoded) -> printJSON decoded
    (errs, []) -> print errs -- TODO exitcode

printJSON :: [Interface] -> IO ()
printJSON = print . Aeson.encode
