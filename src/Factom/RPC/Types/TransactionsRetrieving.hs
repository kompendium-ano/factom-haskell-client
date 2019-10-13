{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module TransactionsRetrieving where

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


data InputsElt = InputsElt {
    inputsEltAmount  :: Double,
    inputsEltAddress :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON InputsElt where
  parseJSON (Object v) = InputsElt <$> v .: "amount" <*> v .: "address"
  parseJSON _          = mzero


instance ToJSON InputsElt where
  toJSON (InputsElt {..}) =
    object ["amount" .= inputsEltAmount, "address" .= inputsEltAddress]
  toEncoding (InputsElt {..}) =
    pairs ("amount" .= inputsEltAmount <> "address" .= inputsEltAddress)


data TransactionsElt = TransactionsElt {
    transactionsEltEcoutputs      :: [InputsElt],
    transactionsEltSigned         :: Bool,
    transactionsEltInputs         :: [InputsElt],
    transactionsEltOutputs        :: (Maybe Value),
    transactionsEltTotalinputs    :: Double,
    transactionsEltTotalecoutputs :: Double,
    transactionsEltTimestamp      :: Double,
    transactionsEltBlockheight    :: Double,
    transactionsEltTotaloutputs   :: Double,
    transactionsEltTxid           :: Text,
    transactionsEltFeespaid       :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TransactionsElt where
  parseJSON (Object v) =
    TransactionsElt
      <$>  v
      .:   "ecoutputs"
      <*>  v
      .:   "signed"
      <*>  v
      .:   "inputs"
      <*>  v
      .:?? "outputs"
      <*>  v
      .:   "totalinputs"
      <*>  v
      .:   "totalecoutputs"
      <*>  v
      .:   "timestamp"
      <*>  v
      .:   "blockheight"
      <*>  v
      .:   "totaloutputs"
      <*>  v
      .:   "txid"
      <*>  v
      .:   "feespaid"
  parseJSON _ = mzero


instance ToJSON TransactionsElt where
  toJSON (TransactionsElt {..}) = object
    [ "ecoutputs" .= transactionsEltEcoutputs
    , "signed" .= transactionsEltSigned
    , "inputs" .= transactionsEltInputs
    , "outputs" .= transactionsEltOutputs
    , "totalinputs" .= transactionsEltTotalinputs
    , "totalecoutputs" .= transactionsEltTotalecoutputs
    , "timestamp" .= transactionsEltTimestamp
    , "blockheight" .= transactionsEltBlockheight
    , "totaloutputs" .= transactionsEltTotaloutputs
    , "txid" .= transactionsEltTxid
    , "feespaid" .= transactionsEltFeespaid
    ]
  toEncoding (TransactionsElt {..}) = pairs
    (  "ecoutputs"
    .= transactionsEltEcoutputs
    <> "signed"
    .= transactionsEltSigned
    <> "inputs"
    .= transactionsEltInputs
    <> "outputs"
    .= transactionsEltOutputs
    <> "totalinputs"
    .= transactionsEltTotalinputs
    <> "totalecoutputs"
    .= transactionsEltTotalecoutputs
    <> "timestamp"
    .= transactionsEltTimestamp
    <> "blockheight"
    .= transactionsEltBlockheight
    <> "totaloutputs"
    .= transactionsEltTotaloutputs
    <> "txid"
    .= transactionsEltTxid
    <> "feespaid"
    .= transactionsEltFeespaid
    )


data TopLevel = TopLevel {
    topLevelTransactions :: [TransactionsElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .: "transactions"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object ["transactions" .= topLevelTransactions]
  toEncoding (TopLevel {..}) = pairs ("transactions" .= topLevelTransactions)
