module Data.Aeson.Extra
  ( mergeObjects
  ) where

import Data.Aeson (Value(Object))
import qualified Data.HashMap.Lazy as HML

mergeObjects :: [Value] -> Value
mergeObjects = Object . HML.unions . map (\(Object x) -> x)
