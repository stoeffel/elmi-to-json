{-# LANGUAGE ApplicativeDo #-}

module Args
  ( parse
  , Args(..)
  ) where

import Control.Applicative (many)
import Data.Semigroup ((<>))
import qualified Options.Applicative as A
import Subset (Subset)
import qualified Subset

newtype Args = Args
  { infoFor :: Subset FilePath
  }

parse :: IO Args
parse =
  A.execParser $
  A.info
    (A.helper <*> parser)
    (A.fullDesc <> A.progDesc "Get info for specific modules." <>
     A.header "elmi-to-json - Convert the interface info into json.")

parser :: A.Parser Args
parser = do
  modulePaths <-
    many $
    A.argument
      A.str
      (A.metavar "MODULE_PATHS" <> A.help "Get info for specific modules.")
  return Args {infoFor = Subset.allIfEmpty modulePaths}
