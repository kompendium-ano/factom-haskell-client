{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module  Factom.RPC.Types.CommitEntry where

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


data CommitEntry = CommitEntry {
    ceChainid   :: Text,
    ceEntryhash :: Text,
    ceMessage   :: Text,
    ceTxid      :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON CommitEntry where
  parseJSON (Object v) =
    CommitEntry
      <$> v
      .:  "chainid"
      <*> v
      .:  "entryhash"
      <*> v
      .:  "message"
      <*> v
      .:  "txid"
  parseJSON _ = mzero


instance ToJSON CommitEntry where
  toJSON (CommitEntry {..}) = object
    [ "chainid" .= ceChainid
    , "entryhash" .= ceEntryhash
    , "message" .= ceMessage
    , "txid" .= ceTxid
    ]
  toEncoding (CommitEntry {..}) = pairs
    (  "chainid"
    .= ceChainid
    <> "entryhash"
    .= ceEntryhash
    <> "message"
    .= ceMessage
    <> "txid"
    .= ceTxid
    )
