{-# LANGUAGE ApplicativeDo #-}

module Options
  ( parse,
    Options (..),
    Mode (..),
    RunMode (..),
    ElmVersion (..),
    Output (..),
  )
where

import Data.Semigroup ((<>))
import qualified Data.String
import qualified Data.Text as T
import qualified Options.Applicative as Opts
import Options.Applicative (help, long, metavar, short, str)

data Mode = Version | Run RunMode Options

data RunMode
  = Normal
  | ForElmTest

data Options
  = Options
      { files :: [FilePath],
        output :: Output,
        elmVersion :: ElmVersion
      }

data Output
  = OutputFile FilePath
  | OutputStdout

data ElmVersion
  = ElmVersion T.Text
  | FromElmJson

parse :: IO Mode
parse = do
  (options, runMode, version) <-
    Opts.execParser $
      Opts.info
        (Opts.helper <*> parser)
        ( Opts.fullDesc <> Opts.progDesc "Get info for specific modules."
            <> Opts.header "elmi-to-json - Convert the interface info into json."
        )
  if version
    then return Version
    else return (Run runMode options)

parser :: Opts.Parser (Options, RunMode, Bool)
parser = do
  files <-
    positionalArguments
      [metavar "MODULE_PATHS", help "Get info for specific modules."]
  output <-
    optional
      OutputStdout
      OutputFile
      [long "output", short 'o', help "Output info to a file."]
  elmVersion <-
    optional
      FromElmJson
      ElmVersion
      [ long "elm-version"
          <> help "Specify elm-compiler version used to compile your code. Default: defined in elm.json or highest found in elm-stuff."
      ]
  runMode <-
    Opts.flag
      Normal
      ForElmTest
      $ mconcat
        [ long "for-elm-test",
          help "Specialized output for internal usage in elm-test."
        ]
  version <-
    Opts.switch $
      mconcat
        [long "version", short 'v', help "Display elmi-to-json version."]
  return (Options {files, output, elmVersion}, runMode, version)

positionalArguments :: [Opts.Mod Opts.ArgumentFields FilePath] -> Opts.Parser [FilePath]
positionalArguments = Opts.many . Opts.argument str . mconcat

optional ::
  Data.String.IsString a =>
  b ->
  (a -> b) ->
  [Opts.Mod Opts.OptionFields a] ->
  Opts.Parser b
optional default' wrap = fmap (maybe default' wrap) . Opts.optional . Opts.strOption . mconcat
