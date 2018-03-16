module Lib
  ( run
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import Elm.Interface (Interface)

run :: IO ()
run = do
  result <- B.decodeFileOrFail "./TODO.elmi"
  case result of
    Left (_, err) -> print err -- TODO exitcode
    Right decoded -> printJSON decoded

printJSON :: Interface -> IO ()
printJSON = print . Aeson.encode
