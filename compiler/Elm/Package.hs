{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Elm.Package
  ( Name(..)
  ) where

import Control.Monad (liftM2)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary, get, put)
import Data.Text (Text)
import GHC.Generics (Generic)

-- PACKGE NAMES
data Name = Name
  { _user :: !Text
  , _project :: !Text
  } deriving (Generic, Show)

-- JSON
instance Aeson.ToJSON Name

-- BINARY
instance Binary Name where
  get = liftM2 Name get get
  put (Name user project) = do
    put user
    put project
