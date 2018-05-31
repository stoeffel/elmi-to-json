{-# OPTIONS_GHC -Wall #-}

module Elm.Interface
  ( Interface(..)
  , Binop(..)
  ) where

import Control.Monad (liftM4)
import Data.Aeson ((.=))
import Data.Binary
import qualified Data.Map as Map
import GHC.Generics (Generic)

import qualified AST.Canonical as Can
import qualified AST.Utils.Binop as Binop
import qualified Data.Aeson as Aeson
import qualified Elm.Name as N

-- INTERFACE
data Interface = Interface
  { _types :: Map.Map N.Name Can.Annotation
  , _unions :: Map.Map N.Name Union
  , _aliases :: Map.Map N.Name Alias
  , _binops :: Map.Map N.Name Binop
  }

data Union
  = OpenUnion Can.Union
  | ClosedUnion Can.Union
  | PrivateUnion Can.Union

data Alias
  = PublicAlias Can.Alias
  | PrivateAlias Can.Alias

data Binop = Binop
  { _op_name :: N.Name
  , _op_annotation :: Can.Annotation
  , _op_associativity :: Binop.Associativity
  , _op_precedence :: Binop.Precedence
  }

-- JSON
instance Aeson.ToJSON Interface where
  toJSON (Interface {_types, _unions, _aliases, _binops}) =
    Aeson.object
      [ "types" .= Aeson.toJSON _types
      , "unions" .= Aeson.toJSON _unions
      , "aliases" .= Aeson.toJSON _aliases
      , "binops" .= Aeson.toJSON _binops
      ]

data Scope
  = Private
  | Public (OpenOrClosed)
  deriving (Show, Generic)

instance Aeson.ToJSON Scope

data OpenOrClosed
  = Open
  | Closed
  deriving (Show, Generic)

instance Aeson.ToJSON OpenOrClosed

instance Aeson.ToJSON Union where
  toJSON (OpenUnion u) =
    Aeson.object ["scope" .= Public Open, "definition" .= Aeson.toJSON u]
  toJSON (ClosedUnion u) =
    Aeson.object ["scope" .= Public Closed, "definition" .= Aeson.toJSON u]
  toJSON (PrivateUnion u) =
    Aeson.object ["scope" .= Private, "definition" .= Aeson.toJSON u]

instance Aeson.ToJSON Alias where
  toJSON (PublicAlias a) =
    Aeson.object ["scope" .= Public Open, "definition" .= Aeson.toJSON a]
  toJSON (PrivateAlias a) =
    Aeson.object ["scope" .= Private, "definition" .= Aeson.toJSON a]

instance Aeson.ToJSON Binop where
  toJSON (Binop {_op_name, _op_annotation, _op_associativity, _op_precedence}) =
    Aeson.object
      [ "name" .= Aeson.toJSON _op_name
      , "annotation" .= Aeson.toJSON _op_annotation
      , "associativity" .= Aeson.toJSON _op_associativity
      , "precedence" .= Aeson.toJSON _op_precedence
      ]

-- BINARY
instance Binary Interface where
  get = liftM4 Interface get get get get
  put (Interface a b c d) = put a >> put b >> put c >> put d

instance Binary Union where
  put union =
    case union of
      OpenUnion u -> putWord8 0 >> put u
      ClosedUnion u -> putWord8 1 >> put u
      PrivateUnion u -> putWord8 2 >> put u
  get = do
    n <- getWord8
    case n of
      0 -> OpenUnion <$> get
      1 -> ClosedUnion <$> get
      2 -> PrivateUnion <$> get
      _ -> error "binary encoding of Union was corrupted"

instance Binary Alias where
  put union =
    case union of
      PublicAlias a -> putWord8 0 >> put a
      PrivateAlias a -> putWord8 1 >> put a
  get = do
    n <- getWord8
    case n of
      0 -> PublicAlias <$> get
      1 -> PrivateAlias <$> get
      _ -> error "binary encoding of Alias was corrupted"

instance Binary Binop where
  get = liftM4 Binop get get get get
  put (Binop a b c d) = put a >> put b >> put c >> put d
