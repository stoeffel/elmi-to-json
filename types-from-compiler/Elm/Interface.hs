{-# OPTIONS_GHC -Wall #-}

module Elm.Interface
  ( Interface (..),
    DependencyInterface (..),
    Binop (..),
  )
where

import qualified AST.Canonical as Can
import qualified AST.Utils.Binop as Binop
import Control.Monad (liftM, liftM3, liftM4, liftM5)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Elm.Package as Pkg

-- DEPENDENCY INTERFACE
data DependencyInterface
  = Public Interface
  | Private
      Pkg.Name
      (Map.Map Name.Name Can.Union)
      (Map.Map Name.Name Can.Alias)

-- INTERFACE
data Interface
  = Interface
      { _home :: Pkg.Name,
        _values :: Map.Map Name.Name Can.Annotation,
        _unions :: Map.Map Name.Name Union,
        _aliases :: Map.Map Name.Name Alias,
        _binops :: Map.Map Name.Name Binop
      }

data Union
  = OpenUnion Can.Union
  | ClosedUnion Can.Union
  | PrivateUnion Can.Union

data Alias
  = PublicAlias Can.Alias
  | PrivateAlias Can.Alias

data Binop
  = Binop
      { _op_name :: Name.Name,
        _op_annotation :: Can.Annotation,
        _op_associativity :: Binop.Associativity,
        _op_precedence :: Binop.Precedence
      }

-- JSON
instance Aeson.ToJSON Interface where
  toJSON (Interface {_home, _values, _unions, _aliases, _binops}) =
    Aeson.object
      [ "home" .= Aeson.toJSON _home,
        "values" .= Aeson.toJSON _values,
        "unions" .= Aeson.toJSON _unions,
        "aliases" .= Aeson.toJSON _aliases,
        "binops" .= Aeson.toJSON _binops
      ]

instance Aeson.ToJSON DependencyInterface where
  toJSON (Public interface) = Aeson.object ["public" .= Aeson.toJSON interface]
  toJSON (Private pkgName unions aliases) =
    Aeson.object
      [ "package_name" .= Aeson.toJSON pkgName,
        "unions" .= Aeson.toJSON unions,
        "aliases" .= Aeson.toJSON aliases
      ]

-- This is used in the json output
data Scope
  = ScopePrivate
  | ScopePublic (OpenOrClosed)
  deriving (Show)

instance Aeson.ToJSON Scope where
  toJSON ScopePrivate = "private"
  toJSON (ScopePublic Open) = "public open"
  toJSON (ScopePublic Closed) = "public closed"

data OpenOrClosed
  = Open
  | Closed
  deriving (Show)

instance Aeson.ToJSON Union where
  toJSON (OpenUnion u) =
    Aeson.object ["scope" .= ScopePublic Open, "definition" .= Aeson.toJSON u]
  toJSON (ClosedUnion u) =
    Aeson.object ["scope" .= ScopePublic Closed, "definition" .= Aeson.toJSON u]
  toJSON (PrivateUnion u) =
    Aeson.object ["scope" .= ScopePrivate, "definition" .= Aeson.toJSON u]

instance Aeson.ToJSON Alias where
  toJSON (PublicAlias a) =
    Aeson.object ["scope" .= ScopePublic Open, "definition" .= Aeson.toJSON a]
  toJSON (PrivateAlias a) =
    Aeson.object ["scope" .= ScopePrivate, "definition" .= Aeson.toJSON a]

instance Aeson.ToJSON Binop where
  toJSON (Binop {_op_name, _op_annotation, _op_associativity, _op_precedence}) =
    Aeson.object
      [ "name" .= Aeson.toJSON _op_name,
        "annotation" .= Aeson.toJSON _op_annotation,
        "associativity" .= Aeson.toJSON _op_associativity,
        "precedence" .= Aeson.toJSON _op_precedence
      ]

-- BINARY
instance Binary Interface where

  get = liftM5 Interface get get get get get

  put (Interface a b c d e) = put a >> put b >> put c >> put d >> put e

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
      0 -> liftM PublicAlias get
      1 -> liftM PrivateAlias get
      _ -> fail "binary encoding of Alias was corrupted"

instance Binary Binop where

  get = liftM4 Binop get get get get

  put (Binop a b c d) = put a >> put b >> put c >> put d

instance Binary DependencyInterface where

  put union =
    case union of
      Public a -> putWord8 0 >> put a
      Private a b c -> putWord8 1 >> put a >> put b >> put c

  get = do
    n <- getWord8
    case n of
      0 -> liftM Public get
      1 -> liftM3 Private get get get
      _ -> fail "binary encoding of DependencyInterface was corrupted"
