{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module AddEcOutput where

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
    outputsEltAddress :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON OutputsElt where
  parseJSON (Object v) = OutputsElt <$> v .:   "amount" <*> v .:   "address"
  parseJSON _          = mzero


instance ToJSON OutputsElt where
  toJSON     (OutputsElt {..}) = object ["amount" .= outputsEltAmount, "address" .= outputsEltAddress]
  toEncoding (OutputsElt {..}) = pairs  ("amount" .= outputsEltAmount<>"address" .= outputsEltAddress)


data TopLevel = TopLevel { 
    topLevelFeesrequired :: Double,
    topLevelEcoutputs :: [OutputsElt],
    topLevelSigned :: Bool,
    topLevelInputs :: [OutputsElt],
    topLevelOutputs :: [OutputsElt],
    topLevelName :: Text,
    topLevelTotalinputs :: Double,
    topLevelTotalecoutputs :: Double,
    topLevelTimestamp :: Double,
    topLevelTotaloutputs :: Double,
    topLevelTxid :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "feesrequired" <*> v .:   "ecoutputs" <*> v .:   "signed" <*> v .:   "inputs" <*> v .:   "outputs" <*> v .:   "name" <*> v .:   "totalinputs" <*> v .:   "totalecoutputs" <*> v .:   "timestamp" <*> v .:   "totaloutputs" <*> v .:   "txid"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["feesrequired" .= topLevelFeesrequired, "ecoutputs" .= topLevelEcoutputs, "signed" .= topLevelSigned, "inputs" .= topLevelInputs, "outputs" .= topLevelOutputs, "name" .= topLevelName, "totalinputs" .= topLevelTotalinputs, "totalecoutputs" .= topLevelTotalecoutputs, "timestamp" .= topLevelTimestamp, "totaloutputs" .= topLevelTotaloutputs, "txid" .= topLevelTxid]
  toEncoding (TopLevel {..}) = pairs  ("feesrequired" .= topLevelFeesrequired<>"ecoutputs" .= topLevelEcoutputs<>"signed" .= topLevelSigned<>"inputs" .= topLevelInputs<>"outputs" .= topLevelOutputs<>"name" .= topLevelName<>"totalinputs" .= topLevelTotalinputs<>"totalecoutputs" .= topLevelTotalecoutputs<>"timestamp" .= topLevelTimestamp<>"totaloutputs" .= topLevelTotaloutputs<>"txid" .= topLevelTxid)




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


