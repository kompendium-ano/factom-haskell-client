{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module AddInput where

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


data InputsElt = InputsElt { 
    inputsEltAmount :: Double,
    inputsEltAddress :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON InputsElt where
  parseJSON (Object v) = InputsElt <$> v .:   "amount" <*> v .:   "address"
  parseJSON _          = mzero


instance ToJSON InputsElt where
  toJSON     (InputsElt {..}) = object ["amount" .= inputsEltAmount, "address" .= inputsEltAddress]
  toEncoding (InputsElt {..}) = pairs  ("amount" .= inputsEltAmount<>"address" .= inputsEltAddress)


data TopLevel = TopLevel { 
    topLevelFeesrequired :: Double,
    topLevelEcoutputs :: (Maybe Value),
    topLevelSigned :: Bool,
    topLevelInputs :: [InputsElt],
    topLevelOutputs :: (Maybe Value),
    topLevelName :: Text,
    topLevelTotalinputs :: Double,
    topLevelTotalecoutputs :: Double,
    topLevelTimestamp :: Double,
    topLevelTotaloutputs :: Double,
    topLevelTxid :: Text,
    topLevelFeespaid :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "feesrequired" <*> v .:?? "ecoutputs" <*> v .:   "signed" <*> v .:   "inputs" <*> v .:?? "outputs" <*> v .:   "name" <*> v .:   "totalinputs" <*> v .:   "totalecoutputs" <*> v .:   "timestamp" <*> v .:   "totaloutputs" <*> v .:   "txid" <*> v .:   "feespaid"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["feesrequired" .= topLevelFeesrequired, "ecoutputs" .= topLevelEcoutputs, "signed" .= topLevelSigned, "inputs" .= topLevelInputs, "outputs" .= topLevelOutputs, "name" .= topLevelName, "totalinputs" .= topLevelTotalinputs, "totalecoutputs" .= topLevelTotalecoutputs, "timestamp" .= topLevelTimestamp, "totaloutputs" .= topLevelTotaloutputs, "txid" .= topLevelTxid, "feespaid" .= topLevelFeespaid]
  toEncoding (TopLevel {..}) = pairs  ("feesrequired" .= topLevelFeesrequired<>"ecoutputs" .= topLevelEcoutputs<>"signed" .= topLevelSigned<>"inputs" .= topLevelInputs<>"outputs" .= topLevelOutputs<>"name" .= topLevelName<>"totalinputs" .= topLevelTotalinputs<>"totalecoutputs" .= topLevelTotalecoutputs<>"timestamp" .= topLevelTimestamp<>"totaloutputs" .= topLevelTotaloutputs<>"txid" .= topLevelTxid<>"feespaid" .= topLevelFeespaid)




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


