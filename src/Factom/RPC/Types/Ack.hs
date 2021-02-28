{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.Ack where

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

data Commitdata =
  Commitdata
    { commitdataStatus :: Text
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON Commitdata where
  parseJSON (Object v) = Commitdata <$> v .: "status"
  parseJSON _          = mzero

instance ToJSON Commitdata where
  toJSON (Commitdata {..}) = object ["status" .= commitdataStatus]
  toEncoding (Commitdata {..}) = pairs ("status" .= commitdataStatus)

data Ack =
  Ack
    { topLevelCommittxid :: Text
    , topLevelEntryhash  :: Text
    , topLevelEntrydata  :: Commitdata
    , topLevelCommitdata :: Commitdata
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON Ack where
  parseJSON (Object v) =
    Ack <$> v .: "committxid" <*> v .: "entryhash" <*> v .: "entrydata" <*>
    v .: "commitdata"
  parseJSON _ = mzero

instance ToJSON Ack where
  toJSON (Ack {..}) =
    object
      [ "committxid" .= topLevelCommittxid
      , "entryhash" .= topLevelEntryhash
      , "entrydata" .= topLevelEntrydata
      , "commitdata" .= topLevelCommitdata
      ]
  toEncoding (Ack {..}) =
    pairs
      ("committxid" .= topLevelCommittxid <> "entryhash" .= topLevelEntryhash <>
       "entrydata" .=
       topLevelEntrydata <>
       "commitdata" .=
       topLevelCommitdata)
