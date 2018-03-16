{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Elm.Name
  ( Name
  ) where

import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.Text as Text
import GHC.Generics (Generic)

-- NAME
newtype Name = Name
  { _name :: Text.Text
  } deriving (Eq, Ord, Generic, Show)

-- JSON
instance Aeson.ToJSON Name

instance Aeson.ToJSONKey Name

-- INSTANCES
instance Binary Name where
  put (Name name) = put name
  get = Name <$> get
