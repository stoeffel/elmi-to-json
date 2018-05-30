{-# OPTIONS_GHC -Wall #-}

module AST.Utils.Binop
  ( Precedence(..)
  , Associativity(..)
  ) where

import Control.Monad (liftM)
import qualified Data.Aeson as Aeson
import Data.Binary
import GHC.Generics (Generic)
import Prelude hiding (Either(..))



-- BINOP STUFF


newtype Precedence = Precedence Int
  deriving (Generic, Show)


data Associativity
  = Left
  | Non
  | Right
  deriving (Generic, Show)



--JSON


instance Aeson.ToJSON Precedence

instance Aeson.ToJSON Associativity



-- BINARY


instance Binary Precedence where
  get =
    liftM Precedence get

  put (Precedence n) =
    put n


instance Binary Associativity where
  get =
    do  n <- getWord8
        return $
          case n of
            0 -> Left
            1 -> Non
            2 -> Right
            _ -> error "Error reading valid associativity from serialized string"

  put assoc =
    putWord8 $
      case assoc of
        Left  -> 0
        Non   -> 1
        Right -> 2
