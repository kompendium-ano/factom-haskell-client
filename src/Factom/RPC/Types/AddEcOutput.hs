{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module  Factom.RPC.Types.AddEcOutput where

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

data EcOutput = EcOutput {
    ecoAmount  :: Double,
    ecoAddress :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON EcOutput where
  parseJSON (Object v) =
    EcOutput <$>
    v .: "amount" <*>
    v .: "address"
  parseJSON _          = mzero

instance ToJSON EcOutput where
  toJSON (EcOutput {..}) =
    object [ "amount"  .= ecoAmount
           , "address" .= ecoAddress
           ]
  toEncoding (EcOutput {..}) =
    pairs (   "amount"  .= ecoAmount
           <> "address" .= ecoAddress
          )

data Result = Result {
    reFeesrequired   :: Double,
    reEcoutputs      :: [EcOutput],
    reSigned         :: Bool,
    reInputs         :: [EcOutput],
    reOutputs        :: [EcOutput],
    reName           :: Text,
    reTotalinputs    :: Double,
    reTotalecoutputs :: Double,
    reTimestamp      :: Double,
    reTotaloutputs   :: Double,
    reTxid           :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)

instance FromJSON Result where
  parseJSON (Object v) =
    Result
      <$> v
      .:  "feesrequired"
      <*> v
      .:  "ecoutputs"
      <*> v
      .:  "signed"
      <*> v
      .:  "inputs"
      <*> v
      .:  "outputs"
      <*> v
      .:  "name"
      <*> v
      .:  "totalinputs"
      <*> v
      .:  "totalecoutputs"
      <*> v
      .:  "timestamp"
      <*> v
      .:  "totaloutputs"
      <*> v
      .:  "txid"
  parseJSON _ = mzero

instance ToJSON Result where
  toJSON (Result {..}) = object
    [ "feesrequired" .= reFeesrequired
    , "ecoutputs" .= reEcoutputs
    , "signed" .= reSigned
    , "inputs" .= reInputs
    , "outputs" .= reOutputs
    , "name" .= reName
    , "totalinputs" .= reTotalinputs
    , "totalecoutputs" .= reTotalecoutputs
    , "timestamp" .= reTimestamp
    , "totaloutputs" .= reTotaloutputs
    , "txid" .= reTxid
    ]
  toEncoding (Result {..}) = pairs
    (  "feesrequired"
    .= reFeesrequired
    <> "ecoutputs"
    .= reEcoutputs
    <> "signed"
    .= reSigned
    <> "inputs"
    .= reInputs
    <> "outputs"
    .= reOutputs
    <> "name"
    .= reName
    <> "totalinputs"
    .= reTotalinputs
    <> "totalecoutputs"
    .= reTotalecoutputs
    <> "timestamp"
    .= reTimestamp
    <> "totaloutputs"
    .= reTotaloutputs
    <> "txid"
    .= reTxid
    )
