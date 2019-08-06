{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Elm.Details
  ( Details(..)
  , Interfaces
  ) where

import Control.Monad (liftM, liftM2, liftM3)
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Elm.Interface as Interface
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File

-- DETAILS
data Details = Details
  { _outlineTime :: File.Time
  , _outline :: ValidOutline
  , _buildID :: BuildID
  , _locals :: Map.Map Name.Name Local
  , _foreigns :: Map.Map Name.Name Foreign
  , _extras :: Extras
  }

type BuildID = Word64

data ValidOutline
  = ValidApp (NE.List Outline.SrcDir)
  | ValidPkg Pkg.Name
             [Name.Name]
             (Map.Map Pkg.Name V.Version) {- for docs in reactor -}

data Local = Local
  { _path :: FilePath
  , _time :: File.Time
  , _deps :: [Name.Name]
  , _main :: Bool
  , _lastChange :: BuildID
  , _lastCompile :: BuildID
  }

data Foreign =
  Foreign Pkg.Name
          [Pkg.Name]

data Extras =
  ArtifactsCached

type Interfaces = Map.Map ModuleName.Canonical Interface.DependencyInterface

instance Binary Details where
  put (Details a b c d e _) = put a >> put b >> put c >> put d >> put e
  get = do
    a <- get
    b <- get
    c <- get
    d <- get
    e <- get
    return (Details a b c d e ArtifactsCached)

instance Binary ValidOutline where
  put outline =
    case outline of
      ValidApp a -> putWord8 0 >> put a
      ValidPkg a b c -> putWord8 1 >> put a >> put b >> put c
  get = do
    n <- getWord8
    case n of
      0 -> liftM ValidApp get
      1 -> liftM3 ValidPkg get get get
      _ -> fail "binary encoding of ValidOutline was corrupted"

instance Binary Local where
  put (Local a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get = do
    a <- get
    b <- get
    c <- get
    d <- get
    e <- get
    f <- get
    return (Local a b c d e f)

instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b
