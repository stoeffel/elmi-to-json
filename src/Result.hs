module Result (Result (..)) where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Info

data Result
  = Result
      { dependencies :: [Info.Dependency],
        details :: Info.Details,
        internals :: [Info.Internal]
      }
  deriving (Generic)

instance Aeson.ToJSON Result
