{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.Heights where

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

data Heights =
  Heights
    { hLeaderheight         :: Double
    , hDirectoryblockheight :: Double
    , hEntryblockheight     :: Double
    , hEntryheight          :: Double
    } deriving (Show, Eq, GHC.Generics.Generic)


instance FromJSON Heights where
  parseJSON (Object v) =
    Heights
      <$> v .: "leaderheight"
      <*> v .: "directoryblockheight"
      <*> v .: "entryblockheight"
      <*> v .: "entryheight"
  parseJSON _          = mzero

instance ToJSON Heights where
  toJSON (Heights {..}) =
    object [ "leaderheight"         .= hLeaderheight
           , "directoryblockheight" .= hDirectoryblockheight
           , "entryblockheight"     .= hEntryblockheight
           , "entryheight"          .= hEntryheight]
  toEncoding (Heights {..}) =
    pairs  (   "leaderheight"         .= hLeaderheight
            <> "directoryblockheight" .= hDirectoryblockheight
            <> "entryblockheight"     .= hEntryblockheight
            <> "entryheight"          .= hEntryheight)
