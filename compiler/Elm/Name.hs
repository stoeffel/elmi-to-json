{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Elm.Name
  ( Name(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import Data.Binary
import qualified Data.Text as Text

-- NAME
newtype Name = Name
  { _name :: Text.Text
  } deriving (Ord, Eq, Show)

-- JSON
instance Aeson.ToJSON Name where
  toJSON = Aeson.String . _name

instance Aeson.ToJSONKey Name where
  toJSONKey = Aeson.ToJSONKeyText _name (Encoding.text . _name)

-- INSTANCES
instance Binary Name where
  put (Name name) = put name
  get = Name <$> get
