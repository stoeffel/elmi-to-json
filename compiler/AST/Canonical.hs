{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Canonical
  ( Annotation(..)
  , Alias(..)
  , Union(..)
  ) where

import Control.Monad (liftM, liftM2, liftM3, liftM4, replicateM)
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.Map as Map
import GHC.Generics (Generic)

import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Name as N

-- TYPES
data Annotation =
  Forall FreeVars
         Type
  deriving (Generic, Show)

type FreeVars = Map.Map N.Name ()

data Type
  = TLambda Type
            Type
  | TVar N.Name
  | TType ModuleName.Canonical
          N.Name
          [Type]
  | TRecord (Map.Map N.Name Type)
            (Maybe N.Name)
  | TUnit
  | TTuple Type
           Type
           (Maybe Type)
  | TAlias ModuleName.Canonical
           N.Name
           [(N.Name, Type)]
           AliasType
  deriving (Generic, Show)

data AliasType
  = Holey Type
  | Filled Type
  deriving (Generic, Show)

data Alias =
  Alias [N.Name]
        Type
        (Maybe [(N.Name, Type)])
  deriving (Generic, Show)

data Union = Union
  { _u_vars :: [N.Name]
  , _u_alts :: [Ctor]
  , _u_numAlts :: Int -- CACHE numAlts for exhaustiveness checking
  , _u_opts :: CtorOpts -- CACHE which optimizations are available
  } deriving (Generic, Show)

data CtorOpts
  = Normal
  | Enum
  | Unbox
  deriving (Generic, Show)

data Ctor =
  Ctor N.Name
       Index.ZeroBased
       Int
       [Type] -- CACHE length args
  deriving (Generic, Show)

-- JSON
instance Aeson.ToJSON Annotation

instance Aeson.ToJSON Type

instance Aeson.ToJSON AliasType

instance Aeson.ToJSON Alias

instance Aeson.ToJSON Union

instance Aeson.ToJSON CtorOpts

instance Aeson.ToJSON Ctor

-- BINARY
instance Binary Alias where
  get = liftM3 Alias get get get
  put (Alias a b c) = put a >> put b >> put c

instance Binary Union where
  put (Union a b c d) = put a >> put b >> put c >> put d
  get = liftM4 Union get get get get

instance Binary Ctor where
  get = liftM4 Ctor get get get get
  put (Ctor a b c d) = put a >> put b >> put c >> put d

instance Binary CtorOpts where
  put opts =
    case opts of
      Normal -> putWord8 0
      Enum -> putWord8 1
      Unbox -> putWord8 2
  get = do
    n <- getWord8
    case n of
      0 -> return Normal
      1 -> return Enum
      2 -> return Unbox
      _ -> error "binary encoding of CtorOpts was corrupted"

instance Binary Annotation where
  get = liftM2 Forall get get
  put (Forall a b) = put a >> put b

instance Binary Type where
  put tipe =
    case tipe of
      TLambda a b -> putWord8 0 >> put a >> put b
      TVar a -> putWord8 1 >> put a
      TRecord a b -> putWord8 2 >> put a >> put b
      TUnit -> putWord8 3
      TTuple a b c -> putWord8 4 >> put a >> put b >> put c
      TAlias a b c d -> putWord8 5 >> put a >> put b >> put c >> put d
      TType home name ts ->
        let potentialWord = length ts + 7
        in if potentialWord <= fromIntegral (maxBound :: Word8)
             then do
               putWord8 (fromIntegral potentialWord)
               put home
               put name
               mapM_ put ts
             else putWord8 6 >> put home >> put name >> put ts
  get = do
    word <- getWord8
    case word of
      0 -> liftM2 TLambda get get
      1 -> liftM TVar get
      2 -> liftM2 TRecord get get
      3 -> return TUnit
      4 -> liftM3 TTuple get get get
      5 -> liftM4 TAlias get get get get
      6 -> liftM3 TType get get get
      n -> liftM3 TType get get (replicateM (fromIntegral (n - 7)) get)

instance Binary AliasType where
  put aliasType =
    case aliasType of
      Holey tipe -> putWord8 0 >> put tipe
      Filled tipe -> putWord8 1 >> put tipe
  get = do
    n <- getWord8
    case n of
      0 -> liftM Holey get
      1 -> liftM Filled get
      _ -> error "binary encoding of AliasType was corrupted"
