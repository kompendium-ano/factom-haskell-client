{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module PendingTransactions where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data OutputsElt = OutputsElt { 
    outputsEltAmount :: Double,
    outputsEltUseraddress :: Text,
    outputsEltAddress :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON OutputsElt where
  parseJSON (Object v) = OutputsElt <$> v .:   "amount" <*> v .:   "useraddress" <*> v .:   "address"
  parseJSON _          = mzero


instance ToJSON OutputsElt where
  toJSON     (OutputsElt {..}) = object ["amount" .= outputsEltAmount, "useraddress" .= outputsEltUseraddress, "address" .= outputsEltAddress]
  toEncoding (OutputsElt {..}) = pairs  ("amount" .= outputsEltAmount<>"useraddress" .= outputsEltUseraddress<>"address" .= outputsEltAddress)


data TopLevelElt = TopLevelElt { 
    topLevelEltStatus :: Text,
    topLevelEltEcoutputs :: [(Maybe Value)],
    topLevelEltFees :: Double,
    topLevelEltInputs :: [OutputsElt],
    topLevelEltTransactionid :: Text,
    topLevelEltOutputs :: [OutputsElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevelElt where
  parseJSON (Object v) = TopLevelElt <$> v .:   "status" <*> v .:   "ecoutputs" <*> v .:   "fees" <*> v .:   "inputs" <*> v .:   "transactionid" <*> v .:   "outputs"
  parseJSON _          = mzero


instance ToJSON TopLevelElt where
  toJSON     (TopLevelElt {..}) = object ["status" .= topLevelEltStatus, "ecoutputs" .= topLevelEltEcoutputs, "fees" .= topLevelEltFees, "inputs" .= topLevelEltInputs, "transactionid" .= topLevelEltTransactionid, "outputs" .= topLevelEltOutputs]
  toEncoding (TopLevelElt {..}) = pairs  ("status" .= topLevelEltStatus<>"ecoutputs" .= topLevelEltEcoutputs<>"fees" .= topLevelEltFees<>"inputs" .= topLevelEltInputs<>"transactionid" .= topLevelEltTransactionid<>"outputs" .= topLevelEltOutputs)


type TopLevel = [TopLevelElt]



parse :: FilePath -> IO TopLevel
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


