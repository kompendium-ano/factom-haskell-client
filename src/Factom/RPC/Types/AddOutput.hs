{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.AddOutput where

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

data OutputsElt =
  OutputsElt
    { outputsEltAmount  :: Double
    , outputsEltAddress :: Text
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON OutputsElt where
  parseJSON (Object v) = OutputsElt <$> v .: "amount" <*> v .: "address"
  parseJSON _          = mzero

instance ToJSON OutputsElt where
  toJSON (OutputsElt {..}) =
    object ["amount" .= outputsEltAmount, "address" .= outputsEltAddress]
  toEncoding (OutputsElt {..}) =
    pairs ("amount" .= outputsEltAmount <> "address" .= outputsEltAddress)

data TopLevel =
  TopLevel
    { topLevelFeesrequired   :: Double
    , topLevelEcoutputs      :: (Maybe Value)
    , topLevelSigned         :: Bool
    , topLevelInputs         :: [OutputsElt]
    , topLevelOutputs        :: [OutputsElt]
    , topLevelName           :: Text
    , topLevelTotalinputs    :: Double
    , topLevelTotalecoutputs :: Double
    , topLevelTimestamp      :: Double
    , topLevelTotaloutputs   :: Double
    , topLevelTxid           :: Text
    , topLevelFeespaid       :: Double
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel <$> v .: "feesrequired" <*> v .:?? "ecoutputs" <*> v .: "signed" <*>
    v .: "inputs" <*>
    v .: "outputs" <*>
    v .: "name" <*>
    v .: "totalinputs" <*>
    v .: "totalecoutputs" <*>
    v .: "timestamp" <*>
    v .: "totaloutputs" <*>
    v .: "txid" <*>
    v .: "feespaid"
  parseJSON _ = mzero

instance ToJSON TopLevel where
  toJSON (TopLevel {..}) =
    object
      [ "feesrequired" .= topLevelFeesrequired
      , "ecoutputs" .= topLevelEcoutputs
      , "signed" .= topLevelSigned
      , "inputs" .= topLevelInputs
      , "outputs" .= topLevelOutputs
      , "name" .= topLevelName
      , "totalinputs" .= topLevelTotalinputs
      , "totalecoutputs" .= topLevelTotalecoutputs
      , "timestamp" .= topLevelTimestamp
      , "totaloutputs" .= topLevelTotaloutputs
      , "txid" .= topLevelTxid
      , "feespaid" .= topLevelFeespaid
      ]
  toEncoding (TopLevel {..}) =
    pairs
      ("feesrequired" .= topLevelFeesrequired <> "ecoutputs" .=
       topLevelEcoutputs <>
       "signed" .=
       topLevelSigned <>
       "inputs" .=
       topLevelInputs <>
       "outputs" .=
       topLevelOutputs <>
       "name" .=
       topLevelName <>
       "totalinputs" .=
       topLevelTotalinputs <>
       "totalecoutputs" .=
       topLevelTotalecoutputs <>
       "timestamp" .=
       topLevelTimestamp <>
       "totaloutputs" .=
       topLevelTotaloutputs <>
       "txid" .=
       topLevelTxid <>
       "feespaid" .=
       topLevelFeespaid)
