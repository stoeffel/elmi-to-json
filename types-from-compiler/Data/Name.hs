{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, FlexibleInstances, MagicHash, UnboxedTuples #-}
module Data.Name
  ( Name
  , toChars
  )
  where


import Prelude hiding (length, maybe, negate)
import qualified Data.Binary as Binary
import qualified Data.String as Chars
import qualified Data.Utf8 as Utf8
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Aeson.Encoding as Encoding



-- NAME


type Name =
  Utf8.Utf8 ELM_NAME


data ELM_NAME



-- INSTANCES


instance Chars.IsString (Utf8.Utf8 ELM_NAME) where
  fromString = Utf8.fromChars
instance Show (Utf8.Utf8 ELM_NAME) where
  show = toChars

instance Binary.Binary (Utf8.Utf8 ELM_NAME) where
  get = Utf8.getUnder256
  put = Utf8.putUnder256


instance Aeson.ToJSON Name where
  toJSON = Aeson.String . T.pack . toChars

instance Aeson.ToJSONKey Name where
  toJSONKey  = Aeson.ToJSONKeyText (T.pack . toChars ) (Encoding.text . T.pack . toChars )

-- TO


toChars :: Name -> [Char]
toChars =
  Utf8.toChars


