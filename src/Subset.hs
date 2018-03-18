module Subset
  ( Subset(..)
  , allIfEmpty
  ) where

data Subset a
  = All
  | Subset [a]

allIfEmpty :: [a] -> Subset a
allIfEmpty subset =
  case subset of
    [] -> All
    xs -> Subset xs
