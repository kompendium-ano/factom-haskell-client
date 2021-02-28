{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.MultipleEcBalances where

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

data TopLevel =
  TopLevel
    { topLevelBalances        :: [BalancesElt]
    , topLevelLastsavedheight :: Double
    , topLevelCurrentheight   :: Double
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel <$> v .: "balances" <*> v .: "lastsavedheight" <*>
    v .: "currentheight"
  parseJSON _ = mzero

instance ToJSON TopLevel where
  toJSON (TopLevel {..}) =
    object
      [ "balances" .= topLevelBalances
      , "lastsavedheight" .= topLevelLastsavedheight
      , "currentheight" .= topLevelCurrentheight
      ]
  toEncoding (TopLevel {..}) =
    pairs
      ("balances" .= topLevelBalances <> "lastsavedheight" .=
       topLevelLastsavedheight <>
       "currentheight" .=
       topLevelCurrentheight)
