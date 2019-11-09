module File
  ( Time,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Fixed as Fixed

-- TIME
newtype Time
  = Time Fixed.Pico
  deriving (Eq, Ord)

instance Aeson.ToJSON Time where
  toJSON (Time time) = Aeson.toJSON time

instance Binary.Binary Time where

  put (Time time) = Binary.put time

  get = Time <$> Binary.get
