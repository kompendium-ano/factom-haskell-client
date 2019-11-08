{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module  Factom.RPC.Types.PendingTransactions where

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


data OutputsElt = OutputsElt {
    outputsEltAmount      :: Double,
    outputsEltUseraddress :: Text,
    outputsEltAddress     :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON OutputsElt where
  parseJSON (Object v) =
    OutputsElt <$> v .: "amount" <*> v .: "useraddress" <*> v .: "address"
  parseJSON _ = mzero


instance ToJSON OutputsElt where
  toJSON (OutputsElt {..}) = object
    [ "amount" .= outputsEltAmount
    , "useraddress" .= outputsEltUseraddress
    , "address" .= outputsEltAddress
    ]
  toEncoding (OutputsElt {..}) = pairs
    (  "amount"
    .= outputsEltAmount
    <> "useraddress"
    .= outputsEltUseraddress
    <> "address"
    .= outputsEltAddress
    )


data TopLevelElt = TopLevelElt {
    topLevelEltStatus        :: Text,
    topLevelEltEcoutputs     :: [(Maybe Value)],
    topLevelEltFees          :: Double,
    topLevelEltInputs        :: [OutputsElt],
    topLevelEltTransactionid :: Text,
    topLevelEltOutputs       :: [OutputsElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevelElt where
  parseJSON (Object v) =
    TopLevelElt
      <$> v
      .:  "status"
      <*> v
      .:  "ecoutputs"
      <*> v
      .:  "fees"
      <*> v
      .:  "inputs"
      <*> v
      .:  "transactionid"
      <*> v
      .:  "outputs"
  parseJSON _ = mzero


instance ToJSON TopLevelElt where
  toJSON (TopLevelElt {..}) = object
    [ "status" .= topLevelEltStatus
    , "ecoutputs" .= topLevelEltEcoutputs
    , "fees" .= topLevelEltFees
    , "inputs" .= topLevelEltInputs
    , "transactionid" .= topLevelEltTransactionid
    , "outputs" .= topLevelEltOutputs
    ]
  toEncoding (TopLevelElt {..}) = pairs
    (  "status"
    .= topLevelEltStatus
    <> "ecoutputs"
    .= topLevelEltEcoutputs
    <> "fees"
    .= topLevelEltFees
    <> "inputs"
    .= topLevelEltInputs
    <> "transactionid"
    .= topLevelEltTransactionid
    <> "outputs"
    .= topLevelEltOutputs
    )


type TopLevel = [TopLevelElt]
