{-# LANGUAGE Trustworthy #-}

-- | Trustworthy wrapper for aeson and aeson-pretty
module Gitson.Json (
  ToJSON, FromJSON, decode, encode
) where

import           Data.Aeson (ToJSON, FromJSON, decode)
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy (ByteString)

encode :: ToJSON a => a -> ByteString
encode = encodePretty' $ Config { confIndent = 2, confCompare = compare }
