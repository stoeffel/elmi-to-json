module Error
  ( Error(..)
  ) where

import qualified Data.Text as T

data Error
  = DecodingElmJsonFailed FilePath
  | ElmStuffNotFound T.Text
  | DecodingElmiFailed String
                       FilePath

instance Show Error where
  show err =
    case err of
      DecodingElmJsonFailed path -> "Couldn't decode " <> path
      ElmStuffNotFound version ->
        "Couldn't find elm-stuff for Elm version " <> T.unpack version <> "."
      DecodingElmiFailed msg path ->
        "Couldn't decode " <> path <>
        ". This file seems to be corrupted. Try to nuke `elm-stuff` and `elm make` again.\n" <>
        "Error: \n" <>
        msg
