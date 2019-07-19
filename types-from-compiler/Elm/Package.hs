{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Elm.Package
  ( Name(..)
  ) where

import Control.Monad (liftM2)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary, get, put)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Utf8 as Utf8
import qualified Elm.Version as V

-- PACKGE NAMES
data Name = Name
  { _author :: !Author
  , _project :: !Project
  } deriving (Eq, Ord)

type Author = Utf8.Utf8 AUTHOR

type Project = Utf8.Utf8 PROJECT

data AUTHOR

data PROJECT

data Package = Package
  { _name :: !Name
  , _version :: !V.Version
  } deriving (Eq, Ord)

toChars :: Name -> String
toChars (Name author project) =
  Utf8.toChars author <> "/" <> Utf8.toChars project

instance Show Name where
  show = toChars

-- JSON
instance Aeson.ToJSON Name where
  toJSON = Aeson.String . T.pack . toChars

-- BINARY
instance Binary Name -- PERF try storing as a Word16
                                                     where
  get = liftM2 Name Utf8.getUnder256 Utf8.getUnder256
  put (Name a b) = Utf8.putUnder256 a >> Utf8.putUnder256 b
