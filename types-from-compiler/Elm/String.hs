{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Elm.String
  ( String,
  )
where

import Data.Binary (Binary, get, put)
import qualified Data.Utf8 as Utf8
import Prelude hiding (String)

-- STRINGS
type String = Utf8.Utf8 ELM_STRING

data ELM_STRING

-- BINARY
instance Binary (Utf8.Utf8 ELM_STRING) where

  get = Utf8.getVeryLong

  put = Utf8.putVeryLong
