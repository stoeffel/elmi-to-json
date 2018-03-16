{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Elm.Package
  ( Name(..)
  ) where

import Control.Monad (liftM2)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary, get, put)
import Data.Semigroup ((<>))
import Data.Text (Text)

-- PACKGE NAMES
data Name = Name
  { _user :: !Text
  , _project :: !Text
  } deriving (Show)

-- JSON
instance Aeson.ToJSON Name where
  toJSON (Name {_user, _project}) = Aeson.String (_user <> "/" <> _project)

-- BINARY
instance Binary Name where
  get = liftM2 Name get get
  put (Name user project) = do
    put user
    put project
