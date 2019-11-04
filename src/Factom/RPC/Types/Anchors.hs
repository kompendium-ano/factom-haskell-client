{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.Anchors where

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


data Bitcoin = Bitcoin {
    bitcoinBlockhash       :: Text,
    bitcoinTransactionhash :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Bitcoin where
  parseJSON (Object v) =
    Bitcoin <$> v .: "blockhash" <*> v .: "transactionhash"
  parseJSON _ = mzero


instance ToJSON Bitcoin where
  toJSON (Bitcoin {..}) = object
    [ "blockhash" .= bitcoinBlockhash
    , "transactionhash" .= bitcoinTransactionhash
    ]
  toEncoding (Bitcoin {..}) = pairs
    (  "blockhash"
    .= bitcoinBlockhash
    <> "transactionhash"
    .= bitcoinTransactionhash
    )


data MerklebranchElt = MerklebranchElt {
    merklebranchEltLeft  :: Text,
    merklebranchEltRight :: Text,
    merklebranchEltTop   :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON MerklebranchElt where
  parseJSON (Object v) =
    MerklebranchElt <$> v .: "left" <*> v .: "right" <*> v .: "top"
  parseJSON _ = mzero


instance ToJSON MerklebranchElt where
  toJSON (MerklebranchElt {..}) = object
    [ "left" .= merklebranchEltLeft
    , "right" .= merklebranchEltRight
    , "top" .= merklebranchEltTop
    ]
  toEncoding (MerklebranchElt {..}) = pairs
    (  "left"
    .= merklebranchEltLeft
    <> "right"
    .= merklebranchEltRight
    <> "top"
    .= merklebranchEltTop
    )


data Ethereum = Ethereum {
    ethereumContractaddress :: Text,
    ethereumTxindex         :: Double,
    ethereumWindowmr        :: Text,
    ethereumBlockhash       :: Text,
    ethereumDbheightmin     :: Double,
    ethereumMerklebranch    :: [MerklebranchElt],
    ethereumRecordheight    :: Double,
    ethereumDbheightmax     :: Double,
    ethereumTxid            :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Ethereum where
  parseJSON (Object v) =
    Ethereum
      <$> v
      .:  "contractaddress"
      <*> v
      .:  "txindex"
      <*> v
      .:  "windowmr"
      <*> v
      .:  "blockhash"
      <*> v
      .:  "dbheightmin"
      <*> v
      .:  "merklebranch"
      <*> v
      .:  "recordheight"
      <*> v
      .:  "dbheightmax"
      <*> v
      .:  "txid"
  parseJSON _ = mzero


instance ToJSON Ethereum where
  toJSON (Ethereum {..}) = object
    [ "contractaddress" .= ethereumContractaddress
    , "txindex" .= ethereumTxindex
    , "windowmr" .= ethereumWindowmr
    , "blockhash" .= ethereumBlockhash
    , "dbheightmin" .= ethereumDbheightmin
    , "merklebranch" .= ethereumMerklebranch
    , "recordheight" .= ethereumRecordheight
    , "dbheightmax" .= ethereumDbheightmax
    , "txid" .= ethereumTxid
    ]
  toEncoding (Ethereum {..}) = pairs
    (  "contractaddress"
    .= ethereumContractaddress
    <> "txindex"
    .= ethereumTxindex
    <> "windowmr"
    .= ethereumWindowmr
    <> "blockhash"
    .= ethereumBlockhash
    <> "dbheightmin"
    .= ethereumDbheightmin
    <> "merklebranch"
    .= ethereumMerklebranch
    <> "recordheight"
    .= ethereumRecordheight
    <> "dbheightmax"
    .= ethereumDbheightmax
    <> "txid"
    .= ethereumTxid
    )


data TopLevel = TopLevel {
    topLevelBitcoin              :: Bitcoin,
    topLevelDirectoryblockheight :: Double,
    topLevelDirectoryblockkeymr  :: Text,
    topLevelEthereum             :: Ethereum
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel
      <$> v
      .:  "bitcoin"
      <*> v
      .:  "directoryblockheight"
      <*> v
      .:  "directoryblockkeymr"
      <*> v
      .:  "ethereum"
  parseJSON _ = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object
    [ "bitcoin" .= topLevelBitcoin
    , "directoryblockheight" .= topLevelDirectoryblockheight
    , "directoryblockkeymr" .= topLevelDirectoryblockkeymr
    , "ethereum" .= topLevelEthereum
    ]
  toEncoding (TopLevel {..}) = pairs
    (  "bitcoin"
    .= topLevelBitcoin
    <> "directoryblockheight"
    .= topLevelDirectoryblockheight
    <> "directoryblockkeymr"
    .= topLevelDirectoryblockkeymr
    <> "ethereum"
    .= topLevelEthereum
    )




parse :: FilePath -> IO TopLevel
parse filename = do
  input <- BSL.readFile filename
  case decode input of
    Nothing -> fatal $ case (decode input :: Maybe Value) of
      Nothing -> "Invalid JSON file: " ++ filename
      Just v  -> "Mismatched JSON value from file: " ++ filename
    Just r -> return (r :: TopLevel)
 where
  fatal :: String -> IO a
  fatal msg = do
    hPutStrLn stderr msg
    exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_
    filenames
    (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
