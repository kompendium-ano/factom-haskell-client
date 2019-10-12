{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module MultipleECBalances where

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


data BalancesElt = BalancesElt { 
    balancesEltAck :: Double,
    balancesEltErr :: Text,
    balancesEltSaved :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON BalancesElt where
  parseJSON (Object v) = BalancesElt <$> v .:   "ack" <*> v .:   "err" <*> v .:   "saved"
  parseJSON _          = mzero


instance ToJSON BalancesElt where
  toJSON     (BalancesElt {..}) = object ["ack" .= balancesEltAck, "err" .= balancesEltErr, "saved" .= balancesEltSaved]
  toEncoding (BalancesElt {..}) = pairs  ("ack" .= balancesEltAck<>"err" .= balancesEltErr<>"saved" .= balancesEltSaved)


data TopLevel = TopLevel { 
    topLevelBalances :: [BalancesElt],
    topLevelLastsavedheight :: Double,
    topLevelCurrentheight :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "balances" <*> v .:   "lastsavedheight" <*> v .:   "currentheight"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["balances" .= topLevelBalances, "lastsavedheight" .= topLevelLastsavedheight, "currentheight" .= topLevelCurrentheight]
  toEncoding (TopLevel {..}) = pairs  ("balances" .= topLevelBalances<>"lastsavedheight" .= topLevelLastsavedheight<>"currentheight" .= topLevelCurrentheight)




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


