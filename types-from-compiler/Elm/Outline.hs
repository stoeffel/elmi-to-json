{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module Elm.Outline
  ( SrcDir(..)
  ) where

import Control.Monad (liftM)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary, get, getWord8, put, putWord8)

data SrcDir
  = AbsoluteSrcDir FilePath
  | RelativeSrcDir FilePath

-- BINARY
instance Binary SrcDir where
  put outline =
    case outline of
      AbsoluteSrcDir a -> putWord8 0 >> put a
      RelativeSrcDir a -> putWord8 1 >> put a
  get = do
    n <- getWord8
    case n of
      0 -> liftM AbsoluteSrcDir get
      1 -> liftM RelativeSrcDir get
      _ -> fail "binary encoding of SrcDir was corrupted"

-- JSON
instance Aeson.ToJSON SrcDir where
  toJSON = Aeson.toJSON . toGiven

toGiven :: SrcDir -> FilePath
toGiven srcDir =
  case srcDir of
    AbsoluteSrcDir dir -> dir
    RelativeSrcDir dir -> dir
