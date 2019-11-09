{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wall #-}

module Elm.Version
  ( Version (..),
  )
where

import Control.Monad (liftM3)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Word (Word16)
import Prelude hiding (max)

-- VERSION
data Version
  = Version
      { _major :: {-# UNPACK #-} !Word16,
        _minor :: {-# UNPACK #-} !Word16,
        _patch :: {-# UNPACK #-} !Word16
      }
  deriving (Eq, Ord)

-- TO CHARS
toChars :: Version -> [Char]
toChars (Version major minor patch) =
  show major ++ '.' : show minor ++ '.' : show patch

-- BINARY
instance Binary Version where

  get = do
    word <- getWord8
    if word == 255
      then liftM3 Version get get get
      else do
        minor <- getWord8
        patch <- getWord8
        return
          (Version (fromIntegral word) (fromIntegral minor) (fromIntegral patch))

  put (Version major minor patch) =
    if major < 255 && minor < 256 && patch < 256
      then do
        putWord8 (fromIntegral major)
        putWord8 (fromIntegral minor)
        putWord8 (fromIntegral patch)
      else do
        putWord8 255
        put major
        put minor
        put patch

-- JSON
instance Aeson.ToJSON Version where
  toJSON = Aeson.toJSON . toChars
