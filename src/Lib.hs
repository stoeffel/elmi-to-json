module Lib
  ( run
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import Elm.Interface (Interface)
import System.Environment (getArgs)

run :: IO ()
run = do
  paths <- getArgs
  result <- traverse B.decodeFileOrFail paths
  case partitionEithers result of
    ([], decoded) -> printJSON decoded
    (errs, []) -> print errs -- TODO exitcode

printJSON :: [Interface] -> IO ()
printJSON = print . Aeson.encode
