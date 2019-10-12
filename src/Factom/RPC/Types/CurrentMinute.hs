{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module CurrentMinute where

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


data TopLevel = TopLevel {
    topLevelCurrenttime             :: Double,
    topLevelLeaderheight            :: Double,
    topLevelDirectoryblockheight    :: Double,
    topLevelStalldetected           :: Bool,
    topLevelFaulttimeout            :: Double,
    topLevelDirectoryblockinseconds :: Double,
    topLevelCurrentminutestarttime  :: Double,
    topLevelCurrentblockstarttime   :: Double,
    topLevelMinute                  :: Double,
    topLevelRoundtimeout            :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel
      <$> v
      .:  "currenttime"
      <*> v
      .:  "leaderheight"
      <*> v
      .:  "directoryblockheight"
      <*> v
      .:  "stalldetected"
      <*> v
      .:  "faulttimeout"
      <*> v
      .:  "directoryblockinseconds"
      <*> v
      .:  "currentminutestarttime"
      <*> v
      .:  "currentblockstarttime"
      <*> v
      .:  "minute"
      <*> v
      .:  "roundtimeout"
  parseJSON _ = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object
    [ "currenttime" .= topLevelCurrenttime
    , "leaderheight" .= topLevelLeaderheight
    , "directoryblockheight" .= topLevelDirectoryblockheight
    , "stalldetected" .= topLevelStalldetected
    , "faulttimeout" .= topLevelFaulttimeout
    , "directoryblockinseconds" .= topLevelDirectoryblockinseconds
    , "currentminutestarttime" .= topLevelCurrentminutestarttime
    , "currentblockstarttime" .= topLevelCurrentblockstarttime
    , "minute" .= topLevelMinute
    , "roundtimeout" .= topLevelRoundtimeout
    ]
  toEncoding (TopLevel {..}) = pairs
    (  "currenttime"
    .= topLevelCurrenttime
    <> "leaderheight"
    .= topLevelLeaderheight
    <> "directoryblockheight"
    .= topLevelDirectoryblockheight
    <> "stalldetected"
    .= topLevelStalldetected
    <> "faulttimeout"
    .= topLevelFaulttimeout
    <> "directoryblockinseconds"
    .= topLevelDirectoryblockinseconds
    <> "currentminutestarttime"
    .= topLevelCurrentminutestarttime
    <> "currentblockstarttime"
    .= topLevelCurrentblockstarttime
    <> "minute"
    .= topLevelMinute
    <> "roundtimeout"
    .= topLevelRoundtimeout
    )
