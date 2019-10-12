{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module WalletBalances where

import           Control.Applicative
import           Control.Monad                  ( forM_
                                                , join
                                                , mzero
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                , decode
                                                , object
                                                , pairs
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Monoid
import           Data.Text                      ( Text )
import qualified GHC.Generics
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data Ecaccountbalances = Ecaccountbalances {
    ecaccountbalancesAck   :: Double,
    ecaccountbalancesSaved :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Ecaccountbalances where
  parseJSON (Object v) = Ecaccountbalances <$> v .: "ack" <*> v .: "saved"
  parseJSON _          = mzero


instance ToJSON Ecaccountbalances where
  toJSON (Ecaccountbalances {..}) =
    object ["ack" .= ecaccountbalancesAck, "saved" .= ecaccountbalancesSaved]
  toEncoding (Ecaccountbalances {..}) =
    pairs ("ack" .= ecaccountbalancesAck <> "saved" .= ecaccountbalancesSaved)


data TopLevel = TopLevel {
    topLevelFctaccountbalances :: Ecaccountbalances,
    topLevelEcaccountbalances  :: Ecaccountbalances
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel <$> v .: "fctaccountbalances" <*> v .: "ecaccountbalances"
  parseJSON _ = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object
    [ "fctaccountbalances" .= topLevelFctaccountbalances
    , "ecaccountbalances" .= topLevelEcaccountbalances
    ]
  toEncoding (TopLevel {..}) = pairs
    (  "fctaccountbalances"
    .= topLevelFctaccountbalances
    <> "ecaccountbalances"
    .= topLevelEcaccountbalances
    )
