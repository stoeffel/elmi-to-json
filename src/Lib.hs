module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import Data.Either (partitionEithers)
import Elm.Interface (Interface)
import qualified Elmi
import Subset (Subset(..))

run :: IO ()
run = do
  Args {infoFor} <- Args.parse
  modulePaths <-
    case infoFor of
      All -> Elmi.all
      Subset modulePaths -> return modulePaths
  result <- traverse (B.decodeFileOrFail . Elmi.fromModulePath) modulePaths
  case partitionEithers result of
    ([], decoded) -> printJSON decoded
    (errs, []) -> print errs -- TODO exitcode
    _ -> putStrLn "failed" -- TODO exitcode

printJSON :: [Interface] -> IO ()
printJSON = print . Aeson.encode
