{-# LANGUAGE ApplicativeDo #-}

module Args
  ( parse
  , Args(..)
  ) where

import Control.Applicative (many, optional)
import Data.Semigroup ((<>))
import qualified Options.Applicative as A
import Subset (Subset)
import qualified Subset

data Args = Args
  { infoFor :: Subset FilePath
  , maybeOutput :: Maybe FilePath
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
  maybeOutput <-
    optional $
    A.strOption
      (A.long "output" <> A.short 'o' <> A.help "Output info to a file.")
  return
    Args {infoFor = Subset.allIfEmpty modulePaths, maybeOutput = maybeOutput}
