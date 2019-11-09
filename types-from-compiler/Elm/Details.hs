{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Elm.Details
  ( Details (..),
    Interfaces,
    ValidOutline,
  )
where

import Control.Monad (liftM, liftM2, liftM3)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Text as T
import qualified Elm.Interface as Interface
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File

-- DETAILS
data Details
  = Details
      { _outlineTime :: File.Time,
        _outline :: ValidOutline,
        _buildID :: BuildID,
        _locals :: Map.Map Name.Name Local,
        _foreigns :: Map.Map Name.Name Foreign,
        _extras :: Extras
      }

type BuildID = Word64

data ValidOutline
  = ValidApp (NE.List Outline.SrcDir)
  | ValidPkg
      Pkg.Name
      [Name.Name]
      (Map.Map Pkg.Name V.Version {- for docs in reactor -})

data Local
  = Local
      { _path :: FilePath,
        _time :: File.Time,
        _deps :: [Name.Name],
        _main :: Bool,
        _lastChange :: BuildID,
        _lastCompile :: BuildID
      }

newtype Foreigns
  = Foreigns
      { unForeigns :: Map.Map Pkg.Name ForeignsInfo
      }

instance Aeson.ToJSON Foreigns where
  toJSON =
    Aeson.toJSON
      . fmap
        ( \(k, ForeignsInfo {dependencies, modules}) ->
            Aeson.object
              [ "pkgName" .= Aeson.toJSON k,
                "dependencies" .= Aeson.toJSON dependencies,
                "modules" .= Aeson.toJSON modules
              ]
        )
      . Map.toList
      . unForeigns

data ForeignsInfo
  = ForeignsInfo
      { dependencies :: [Pkg.Name],
        modules :: [Name.Name]
      }

instance Semigroup ForeignsInfo where
  (ForeignsInfo a b) <> (ForeignsInfo c d) = ForeignsInfo (a <> c) (b <> d)

toForeigns :: Map.Map Name.Name Foreign -> Foreigns
toForeigns =
  Foreigns
    . Map.foldlWithKey
      ( \acc key (Foreign pkgName deps) ->
          snd $
            Map.insertLookupWithKey
              (\_ newValue oldValue -> newValue <> oldValue)
              pkgName
              (ForeignsInfo deps [key])
              acc
      )
      Map.empty

data Foreign
  = Foreign
      Pkg.Name
      [Pkg.Name]

data Extras
  = ArtifactsCached

type Interfaces = Map.Map ModuleName.Canonical Interface.DependencyInterface

instance Aeson.ToJSON Details where
  toJSON
    ( Details
        { _outlineTime,
          _outline,
          _buildID,
          _locals,
          _foreigns,
          _extras
        }
      ) =
      Aeson.object
        [ "outlineTime" .= Aeson.toJSON _outlineTime,
          "outline" .= Aeson.toJSON _outline,
          "buildID" .= Aeson.toJSON _buildID,
          "locals" .= Aeson.toJSON _locals,
          "foreigns" .= (Aeson.toJSON $ toForeigns _foreigns),
          "extras" .= ("artifactsCached" :: String)
        ]

instance Aeson.ToJSON Local where
  toJSON (Local {_path, _time, _deps, _main, _lastChange, _lastCompile}) =
    Aeson.object
      [ "path" .= Aeson.toJSON _path,
        "time" .= Aeson.toJSON _time,
        "deps" .= Aeson.toJSON _deps,
        "main" .= Aeson.toJSON _main,
        "lastChange" .= Aeson.toJSON _lastChange,
        "lastCompile" .= Aeson.toJSON _lastCompile
      ]

instance Aeson.ToJSON Foreign where
  toJSON (Foreign pkgName pkgNames) =
    Aeson.object [(T.pack $ show pkgName) .= Aeson.toJSON pkgNames]

instance Aeson.ToJSON ValidOutline where
  toJSON (ValidApp srcDirs) =
    Aeson.object
      ["type" .= ("ValidApp" :: String), "srcDirs" .= Aeson.toJSON srcDirs]
  toJSON (ValidPkg pkg exposedList exactDeps) =
    Aeson.object
      [ "type" .= ("ValidPkg" :: String),
        "pkg" .= Aeson.toJSON pkg,
        "exposedList" .= Aeson.toJSON exposedList,
        "exactDeps" .= Aeson.toJSON exactDeps
      ]

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
