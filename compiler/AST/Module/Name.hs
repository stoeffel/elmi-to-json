{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Module.Name
  ( Canonical(..)
  ) where

import Control.Monad (liftM2)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Binary

import qualified Elm.Name as N
import qualified Elm.Package as Pkg

-- NAMES
data Canonical = Canonical
  { _package :: !Pkg.Name
  , _module :: !N.Name
  } deriving (Show)

-- JSON
instance Aeson.ToJSON Canonical where
  toJSON (Canonical {_package, _module}) =
    Aeson.object ["package" .= _package, "module" .= _module]

-- BINARY
instance Binary Canonical where
  put (Canonical a b) = put a >> put b
  get = liftM2 Canonical get get
