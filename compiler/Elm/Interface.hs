{-# OPTIONS_GHC -Wall #-}

module Elm.Interface
  ( Interface(..)
  , Binop(..)
  ) where

import Control.Monad (liftM4)
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.Map as Map
import GHC.Generics (Generic)

import qualified AST.Canonical as Can
import qualified AST.Utils.Binop as Binop
import qualified Elm.Name as N

-- INTERFACE
data Interface = Interface
  { _types :: Map.Map N.Name Can.Annotation
  , _unions :: Map.Map N.Name Can.Union
  , _aliases :: Map.Map N.Name Can.Alias
  , _binops :: Map.Map N.Name Binop
  } deriving (Generic, Show)

data Binop = Binop
  { _op_name :: N.Name
  , _op_annotation :: Can.Annotation
  , _op_associativity :: Binop.Associativity
  , _op_precedence :: Binop.Precedence
  } deriving (Generic, Show)

-- JSON
instance Aeson.ToJSON Interface

instance Aeson.ToJSON Binop

-- BINARY
instance Binary Interface where
  get = liftM4 Interface get get get get
  put (Interface a b c d) = put a >> put b >> put c >> put d

instance Binary Binop where
  get = liftM4 Binop get get get get
  put (Binop a b c d) = put a >> put b >> put c >> put d
