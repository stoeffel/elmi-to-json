module Subset
  ( Subset(..)
  , allIfEmpty
  ) where

import qualified Data.List as L
import Data.Semigroup ((<>))

data Subset a
  = All
  | Subset [a]

instance Show a => Show (Subset a) where
  show All = "all"
  show (Subset xs) = "subset " <> L.intercalate ", " (fmap show xs)

allIfEmpty :: [a] -> Subset a
allIfEmpty subset =
  case subset of
    [] -> All
    xs -> Subset xs
