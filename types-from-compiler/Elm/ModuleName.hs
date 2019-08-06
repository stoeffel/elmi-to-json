{-# LANGUAGE BangPatterns, OverloadedStrings, UnboxedTuples #-}

module Elm.ModuleName
  ( Canonical(..)
  ) where

import Control.Monad (liftM2)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary(..))
import qualified Data.Name as Name

import qualified Elm.Package as Pkg

-- CANONICAL
data Canonical = Canonical
  { _package :: !Pkg.Name
  , _module :: !Name.Name
  } deriving (Show)

-- JSON
instance Aeson.ToJSON Canonical where
  toJSON (Canonical {_package, _module}) =
    Aeson.object
      ["package" .= Aeson.toJSON _package, "module" .= Aeson.toJSON _module]

-- INSTANCES
instance Eq Canonical where
  (==) (Canonical pkg1 name1) (Canonical pkg2 name2) =
    name1 == name2 && pkg1 == pkg2

instance Ord Canonical where
  compare (Canonical pkg1 name1) (Canonical pkg2 name2) =
    case compare name1 name2 of
      LT -> LT
      EQ -> compare pkg1 pkg2
      GT -> GT

instance Binary Canonical where
  put (Canonical a b) = put a >> put b
  get = liftM2 Canonical get get
