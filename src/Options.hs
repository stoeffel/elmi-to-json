{-# LANGUAGE ApplicativeDo #-}

module Options (parse , Options(..), Mode(..), ElmVersion(..), Output(..)) where

import qualified Data.String
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opts
import Options.Applicative (short, long, help, metavar, str)
import qualified Data.Text as T

data Mode = Version | Run Options

data Options = Options
  { files :: [FilePath]
  , output :: Output
  , elmVersion :: ElmVersion
  }

data Output
  = OutputFile FilePath
  | OutputStdout

data ElmVersion
  = ElmVersion T.Text
  | FromElmJson

parse :: IO Mode
parse = do
  (options, version) <- Opts.execParser $
    Opts.info (Opts.helper <*> parser)
      (Opts.fullDesc <> Opts.progDesc "Get info for specific modules." <>
      Opts.header "elmi-to-json - Convert the interface info into json.")
  if version then
    return Version
  else
    return (Run options)

parser :: Opts.Parser (Options, Bool)
parser = do
  files <- positionalArguments
    [ metavar "MODULE_PATHS", help "Get info for specific modules." ]
  output <- optional OutputStdout OutputFile
    [ long "output", short 'o', help "Output info to a file." ]
  elmVersion <- optional FromElmJson ElmVersion
    [ long "elm-version"
    , help  "Specify elm-compiler version used to compile your code. Default: defined in elm.json or highest found in elm-stuff."
    ]
  version <- Opts.switch $ mconcat
    [ long "version", short 'v', help "Display elmi-to-json version." ]
  return (Options {files, output, elmVersion}, version)

positionalArguments :: [Opts.Mod Opts.ArgumentFields FilePath] -> Opts.Parser [FilePath]
positionalArguments = Opts.many . Opts.argument str . mconcat

optional :: Data.String.IsString a =>
                     b -> (a -> b) -> [Opts.Mod Opts.OptionFields a] -> Opts.Parser b
optional default' wrap = fmap (maybe default' wrap) . Opts.optional . Opts.strOption . mconcat
