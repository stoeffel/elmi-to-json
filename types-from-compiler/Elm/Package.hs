{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Elm.Package
  ( Name(..)
  ) where

import Control.Monad (liftM, liftM2, liftM3)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Word (Word16)



-- PACKGE NAMES


data Name =
  Name
    { _author :: !Text
    , _project :: !Text
    }
    deriving (Eq, Ord, Show)


data Package =
  Package
    { _name :: !Name
    , _version :: !Version
    }
    deriving (Eq, Ord)


data Version =
  Version
    { _major :: {-# UNPACK #-} !Word16
    , _minor :: {-# UNPACK #-} !Word16
    , _patch :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Ord)


-- JSON


instance Aeson.ToJSON Name where
  toJSON (Name {_author, _project}) = Aeson.String (_author <> "/" <> _project)



-- BINARY


instance Binary Name where
  get =
    liftM2 Name get get

  put (Name author project) =
    do  put author
        put project


instance Binary Package where
  get =
    liftM2 Package get get

  put (Package name version) =
    do  put name
        put version


instance Binary Version where
  get =
    do  word <- getWord8
        if word == 0
          then liftM3 Version get get get
          else
            do  minor <- liftM fromIntegral getWord8
                patch <- liftM fromIntegral getWord8
                return (Version (fromIntegral word) minor patch)

  put (Version major minor patch) =
    if major < 256 && minor < 256 && patch < 256 then
      do  putWord8 (fromIntegral major)
          putWord8 (fromIntegral minor)
          putWord8 (fromIntegral patch)
    else
      do  putWord8 0
          put major
          put minor
          put patch
