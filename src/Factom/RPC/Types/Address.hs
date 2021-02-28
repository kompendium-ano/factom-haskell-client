{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.Address where

import           Control.Applicative
import           Control.Monad                   (forM_, join, mzero)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..), decode, object,
                                                  pairs, (.:), (.:?), (.=))
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Monoid
import           Data.Text                       (Text)
import qualified GHC.Generics
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)

data Address =
  Address
    { addressSecret :: Text
    , addressPublic :: Text
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON Address where
  parseJSON (Object v) = Address <$> v .: "secret" <*> v .: "public"
  parseJSON _          = mzero

instance ToJSON Address where
  toJSON (Address {..}) =
    object ["secret" .= addressSecret, "public" .= addressPublic]
  toEncoding (Address {..}) =
    pairs ("secret" .= addressSecret <> "public" .= addressPublic)
