{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module JsonDataFactoidBlock where

import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import qualified Data.ByteString.Lazy.Char8    as BSL
import           System.Environment             ( getArgs )
import           Control.Monad                  ( forM_
                                                , mzero
                                                , join
                                                )
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson                     ( decode
                                                , Value(..)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , pairs
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                , object
                                                )
import           Data.Monoid
import           Data.Text                      ( Text )
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data OutputsElt = OutputsElt {
    outputsEltAmount :: Double,
    outputsEltUseraddress :: Text,
    outputsEltAddress :: Text
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


data SigblocksElt = SigblocksElt {
    sigblocksEltSignatures :: [Text]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SigblocksElt where
  parseJSON (Object v) = SigblocksElt <$> v .: "signatures"
  parseJSON _          = mzero


instance ToJSON SigblocksElt where
  toJSON (SigblocksElt {..}) = object ["signatures" .= sigblocksEltSignatures]
  toEncoding (SigblocksElt {..}) =
    pairs ("signatures" .= sigblocksEltSignatures)


data TransactionsElt = TransactionsElt {
    transactionsEltInputs :: [OutputsElt:|:[(Maybe Value)]],
    transactionsEltOutecs :: [[(Maybe Value)]],
    transactionsEltMillitimestamp :: Double,
    transactionsEltOutputs :: [OutputsElt:|:[(Maybe Value)]],
    transactionsEltSigblocks :: [SigblocksElt:|:[(Maybe Value)]],
    transactionsEltBlockheight :: Double,
    transactionsEltRcds :: [Text:|:[(Maybe Value)]],
    transactionsEltTxid :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TransactionsElt where
  parseJSON (Object v) =
    TransactionsElt
      <$> v
      .:  "inputs"
      <*> v
      .:  "outecs"
      <*> v
      .:  "millitimestamp"
      <*> v
      .:  "outputs"
      <*> v
      .:  "sigblocks"
      <*> v
      .:  "blockheight"
      <*> v
      .:  "rcds"
      <*> v
      .:  "txid"
  parseJSON _ = mzero


instance ToJSON TransactionsElt where
  toJSON (TransactionsElt {..}) = object
    [ "inputs" .= transactionsEltInputs
    , "outecs" .= transactionsEltOutecs
    , "millitimestamp" .= transactionsEltMillitimestamp
    , "outputs" .= transactionsEltOutputs
    , "sigblocks" .= transactionsEltSigblocks
    , "blockheight" .= transactionsEltBlockheight
    , "rcds" .= transactionsEltRcds
    , "txid" .= transactionsEltTxid
    ]
  toEncoding (TransactionsElt {..}) = pairs
    (  "inputs"
    .= transactionsEltInputs
    <> "outecs"
    .= transactionsEltOutecs
    <> "millitimestamp"
    .= transactionsEltMillitimestamp
    <> "outputs"
    .= transactionsEltOutputs
    <> "sigblocks"
    .= transactionsEltSigblocks
    <> "blockheight"
    .= transactionsEltBlockheight
    <> "rcds"
    .= transactionsEltRcds
    <> "txid"
    .= transactionsEltTxid
    )


data Fblock = Fblock {
    fblockBodymr :: Text,
    fblockTransactions :: [TransactionsElt],
    fblockChainid :: Text,
    fblockDbheight :: Double,
    fblockPrevledgerkeymr :: Text,
    fblockExchrate :: Double,
    fblockLedgerkeymr :: Text,
    fblockKeymr :: Text,
    fblockPrevkeymr :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Fblock where
  parseJSON (Object v) =
    Fblock
      <$> v
      .:  "bodymr"
      <*> v
      .:  "transactions"
      <*> v
      .:  "chainid"
      <*> v
      .:  "dbheight"
      <*> v
      .:  "prevledgerkeymr"
      <*> v
      .:  "exchrate"
      <*> v
      .:  "ledgerkeymr"
      <*> v
      .:  "keymr"
      <*> v
      .:  "prevkeymr"
  parseJSON _ = mzero


instance ToJSON Fblock where
  toJSON (Fblock {..}) = object
    [ "bodymr" .= fblockBodymr
    , "transactions" .= fblockTransactions
    , "chainid" .= fblockChainid
    , "dbheight" .= fblockDbheight
    , "prevledgerkeymr" .= fblockPrevledgerkeymr
    , "exchrate" .= fblockExchrate
    , "ledgerkeymr" .= fblockLedgerkeymr
    , "keymr" .= fblockKeymr
    , "prevkeymr" .= fblockPrevkeymr
    ]
  toEncoding (Fblock {..}) = pairs
    (  "bodymr"
    .= fblockBodymr
    <> "transactions"
    .= fblockTransactions
    <> "chainid"
    .= fblockChainid
    <> "dbheight"
    .= fblockDbheight
    <> "prevledgerkeymr"
    .= fblockPrevledgerkeymr
    <> "exchrate"
    .= fblockExchrate
    <> "ledgerkeymr"
    .= fblockLedgerkeymr
    <> "keymr"
    .= fblockKeymr
    <> "prevkeymr"
    .= fblockPrevkeymr
    )


data TopLevel = TopLevel {
    topLevelRawdata :: Text,
    topLevelFblock :: Fblock
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .: "rawdata" <*> v .: "fblock"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) =
    object ["rawdata" .= topLevelRawdata, "fblock" .= topLevelFblock]
  toEncoding (TopLevel {..}) =
    pairs ("rawdata" .= topLevelRawdata <> "fblock" .= topLevelFblock)




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

