{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Info
  ( Dependency(..)
  , Internal(..)
  , Details(..)
  , forInternal
  , forDependencies
  , forDetails
  ) where

import qualified AST.Canonical as Can
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Extra as Aeson
import qualified Data.Binary as B
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Traversable as Traversable
import Data.Typeable (Typeable)
import qualified Elm.Details
import Elm.Interface (Interface)
import qualified Elm.Interface
import qualified Elm.ModuleName as ModuleName
import qualified Elmi
import GHC.Generics (Generic)
import System.FilePath (FilePath)

data Dependency
  = PrivateDependency ModuleName.Canonical
                      (Map.Map Name.Name Can.Union)
                      (Map.Map Name.Name Can.Alias)
  | PublicDependency ModuleName.Canonical
                     Interface
  deriving (Generic)

instance Aeson.ToJSON Dependency where
  toJSON (PrivateDependency moduleName unions aliases) =
    Aeson.mergeObjects
      [ Aeson.object
          [ "scope" .= ("private" :: String)
          , "type" .= ("dependency" :: String)
          , "unions" .= Aeson.toJSON unions
          , "aliases" .= Aeson.toJSON aliases
          ]
      , Aeson.toJSON moduleName
      ]
  toJSON (PublicDependency moduleName interface) =
    Aeson.mergeObjects
      [ Aeson.object
          ["scope" .= ("public" :: String), "type" .= ("dependency" :: String)]
      , Aeson.toJSON moduleName
      , Aeson.toJSON interface
      ]

data Internal = Internal
  { moduleName :: T.Text
  , modulePath :: FilePath
  , interface :: Interface
  } deriving (Generic)

instance Aeson.ToJSON Internal where
  toJSON Internal {moduleName, modulePath, interface} =
    Aeson.mergeObjects
      [ Aeson.object
          [ "module" .= Aeson.toJSON moduleName
          , "path" .= Aeson.toJSON modulePath
          , "scope" .= ("public" :: String)
          , "type" .= ("internal" :: String)
          ]
      , Aeson.toJSON interface
      ]

newtype Details =
  Details Elm.Details.Details
  deriving (Generic)

instance Aeson.ToJSON Details where
  toJSON (Details details) =
    Aeson.mergeObjects
      [Aeson.object ["type" .= ("details" :: String)], Aeson.toJSON details]

forDependencies :: FilePath -> IO [Dependency]
forDependencies dependencyInterfacePath = do
  result <- B.decodeFileOrFail dependencyInterfacePath
  case result of
    Left (_, err) -> ES.throwM (DecodingElmiFailed err dependencyInterfacePath)
    Right (interfaces) ->
      Traversable.for
        (Map.toList (interfaces :: Elm.Details.Interfaces))
        (\(module', i) ->
           case i of
             Elm.Interface.Private _ unions aliases ->
               return $ PrivateDependency module' unions aliases
             Elm.Interface.Public interface ->
               return $ PublicDependency module' interface)

forDetails :: FilePath -> IO Details
forDetails detailsPath = do
  result <- B.decodeFileOrFail detailsPath
  case result of
    Left (_, err) -> ES.throwM (DecodingElmiFailed err detailsPath)
    Right details -> return (Details details)

forInternal :: Elmi.InterfacePaths -> IO Internal
forInternal Elmi.InterfacePaths {Elmi.interfacePath, Elmi.modulePath} = do
  result <- B.decodeFileOrFail interfacePath
  case result of
    Left (_, err) -> ES.throwM (DecodingElmiFailed err interfacePath)
    Right interface ->
      return
        Internal
        { modulePath = modulePath
        , moduleName = Elmi.toModuleName interfacePath
        , interface = interface
        }

data DecodingElmiFailed =
  DecodingElmiFailed String
                     FilePath
  deriving (Typeable, Exception)

instance Show DecodingElmiFailed where
  show (DecodingElmiFailed err path) =
    "Couldn't decode " <> path <>
    ". This file seems to be corrupted. Try to nuke `elm-stuff` and `elm make` again.\n" <>
    "Error: \n" <>
    err
