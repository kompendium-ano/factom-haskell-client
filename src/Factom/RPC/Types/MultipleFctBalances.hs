{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.MultipleFctBalances where

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

data BalancesElt =
  BalancesElt
    { balancesEltAck   :: Double
    , balancesEltErr   :: Text
    , balancesEltSaved :: Double
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON BalancesElt where
  parseJSON (Object v) =
    BalancesElt <$> v .: "ack" <*> v .: "err" <*> v .: "saved"
  parseJSON _ = mzero

instance ToJSON BalancesElt where
  toJSON (BalancesElt {..}) =
    object
      [ "ack" .= balancesEltAck
      , "err" .= balancesEltErr
      , "saved" .= balancesEltSaved
      ]
  toEncoding (BalancesElt {..}) =
    pairs
      ("ack" .= balancesEltAck <> "err" .= balancesEltErr <> "saved" .=
       balancesEltSaved)

data MultipleFCTBalances =
  MultipleFCTBalances
    { mfbBalances        :: [BalancesElt]
    , mfbLastsavedheight :: Double
    , mfbCurrentheight   :: Double
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON MultipleFCTBalances where
  parseJSON (Object v) =
    MultipleFCTBalances <$> v .: "balances" <*> v .: "lastsavedheight" <*>
    v .: "currentheight"
  parseJSON _ = mzero

instance ToJSON MultipleFCTBalances where
  toJSON (MultipleFCTBalances {..}) =
    object
      [ "balances" .= mfbBalances
      , "lastsavedheight" .= mfbLastsavedheight
      , "currentheight" .= mfbCurrentheight
      ]
  toEncoding (MultipleFCTBalances {..}) =
    pairs
      ("balances" .= mfbBalances <> "lastsavedheight" .= mfbLastsavedheight <>
       "currentheight" .=
       mfbCurrentheight)
