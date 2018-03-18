{-# LANGUAGE ApplicativeDo #-}

module Args
  ( parse
  , Args(..)
  , Subset(..)
  ) where

import Control.Applicative (many)
import Data.Semigroup ((<>))
import Options.Applicative
       (Parser, ParserInfo, argument, execParser, fullDesc, header, help,
        helper, info, metavar, progDesc, str)

newtype Args = Args
  { infoFor :: Subset FilePath
  }

data Subset a
  = All
  | Subset [a]

parse :: IO Args
parse = execParser options

options :: ParserInfo Args
options =
  info
    (helper <*> parser)
    (fullDesc <> progDesc "Print a greeting for TARGET" <>
     header "hello - a test for optparse-applicative")

parser :: Parser Args
parser = do
  modulePaths <-
    many $
    argument
      str
      (metavar "MODULE_PATHS " <> help "Get info for specific modules.")
  return Args {infoFor = allIfEmpty modulePaths}

allIfEmpty :: [a] -> Subset a
allIfEmpty subset =
  case subset of
    [] -> All
    xs -> Subset xs
