{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.Util.UserAgent (
    UserAgent
  , userAgent
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified
import Data.Version qualified
import Paths_blockfrost_api qualified

data UserAgent

userAgent :: ByteString
userAgent =
  "blockfrost-haskell/"
  <> Data.ByteString.Char8.pack
        (Data.Version.showVersion Paths_blockfrost_api.version)
