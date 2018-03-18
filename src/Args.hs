{-# LANGUAGE ApplicativeDo #-}

module Args
  ( parse
  , Args(..)
  ) where

import Control.Applicative (many)
import Data.Semigroup ((<>))
import Data.String (IsString)
import Data.Text as T
import Options.Applicative
       (Mod, OptionFields, Parser, ParserInfo, argument, execParser,
        fullDesc, header, help, helper, idm, info, long, metavar, progDesc,
        str)

data Args = Args
  { modulePaths :: [FilePath] -- TODO make this a union either all or subset
  }

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
  return Args {modulePaths = modulePaths}
