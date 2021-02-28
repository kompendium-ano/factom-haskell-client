{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.CurrentMinute where

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

data CurrentMinute =
  CurrentMinute
    { cmCurrentTime             :: Double
    , cmLeaderHeight            :: Double
    , cmDirectoryBlockHeight    :: Double
    , cmStallDetected           :: Bool
    , cmFaultTimeout            :: Double
    , cmDirectoryBlockInSeconds :: Double
    , cmCurrentMinuteStartTime  :: Double
    , cmCurrentBlockStartTime   :: Double
    , cmMinute                  :: Double
    , cmRoundTimeout            :: Double
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON CurrentMinute where
  parseJSON (Object v) =
    CurrentMinute <$> v .: "currenttime" <*> v .: "leaderheight" <*>
    v .: "directoryblockheight" <*>
    v .: "stalldetected" <*>
    v .: "faulttimeout" <*>
    v .: "directoryblockinseconds" <*>
    v .: "currentminutestarttime" <*>
    v .: "currentblockstarttime" <*>
    v .: "minute" <*>
    v .: "roundtimeout"
  parseJSON _ = mzero

instance ToJSON CurrentMinute where
  toJSON (CurrentMinute {..}) =
    object
      [ "currenttime" .= cmCurrentTime
      , "leaderheight" .= cmLeaderHeight
      , "directoryblockheight" .= cmDirectoryBlockHeight
      , "stalldetected" .= cmStallDetected
      , "faulttimeout" .= cmFaultTimeout
      , "directoryblockinseconds" .= cmDirectoryBlockInSeconds
      , "currentminutestarttime" .= cmCurrentMinuteStartTime
      , "currentblockstarttime" .= cmCurrentBlockStartTime
      , "minute" .= cmMinute
      , "roundtimeout" .= cmRoundTimeout
      ]
  toEncoding (CurrentMinute {..}) =
    pairs
      ("currenttime" .= cmCurrentTime <> "leaderheight" .= cmLeaderHeight <>
       "directoryblockheight" .=
       cmDirectoryBlockHeight <>
       "stalldetected" .=
       cmStallDetected <>
       "faulttimeout" .=
       cmFaultTimeout <>
       "directoryblockinseconds" .=
       cmDirectoryBlockInSeconds <>
       "currentminutestarttime" .=
       cmCurrentMinuteStartTime <>
       "currentblockstarttime" .=
       cmCurrentBlockStartTime <>
       "minute" .=
       cmMinute <>
       "roundtimeout" .=
       cmRoundTimeout)
