{-# OPTIONS_GHC -Wall #-}

module Elm.Interface
  ( Interface(..)
  , Binop(..)
  ) where

import Control.Monad (liftM4)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Utils.Binop as Binop
import qualified Elm.Name as N

-- INTERFACE
data Interface = Interface
  { _types :: Map.Map N.Name Can.Annotation
  , _unions :: Map.Map N.Name Can.Union
  , _aliases :: Map.Map N.Name Can.Alias
  , _binops :: Map.Map N.Name Binop
  } deriving (Show)

data Binop = Binop
  { _op_name :: N.Name
  , _op_annotation :: Can.Annotation
  , _op_associativity :: Binop.Associativity
  , _op_precedence :: Binop.Precedence
  } deriving (Show)

-- JSON
instance Aeson.ToJSON Interface where
  toJSON (Interface {_types, _unions, _aliases, _binops}) =
    Aeson.object
      [ "types" .= Aeson.toJSON _types
      , "unions" .= Aeson.toJSON _unions
      , "aliases" .= Aeson.toJSON _aliases
      , "binops" .= Aeson.toJSON _binops
      ]

instance Aeson.ToJSON Binop where
  toJSON (Binop {_op_name, _op_annotation, _op_associativity, _op_precedence}) =
    Aeson.object
      [ "name" .= Aeson.toJSON _op_name
      , "annotation" .= Aeson.toJSON _op_annotation
      , "associativity" .= Aeson.toJSON _op_associativity
      , "precedence" .= Aeson.toJSON _op_precedence
      ]

-- instance Aeson.ToJSON a => Aeson.ToJSON (Map.Map N.Name a) Interface where
--   toJSON Vk
-- BINARY
instance Binary Interface where
  get = liftM4 Interface get get get get
  put (Interface a b c d) = put a >> put b >> put c >> put d

instance Binary Binop where
  get = liftM4 Binop get get get get
  put (Binop a b c d) = put a >> put b >> put c >> put d
