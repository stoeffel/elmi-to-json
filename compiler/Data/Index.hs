module Data.Index
  ( ZeroBased
  ) where

import Control.Monad (liftM)
import qualified Data.Aeson as Aeson
import Data.Binary
import GHC.Generics (Generic)

-- ZERO BASED
newtype ZeroBased =
  ZeroBased Int
  deriving (Generic, Show)

-- JSON
instance Aeson.ToJSON ZeroBased

-- BINARY
instance Binary ZeroBased where
  get = liftM ZeroBased get
  put (ZeroBased n) = put n
