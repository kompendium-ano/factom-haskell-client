{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module  Factom.RPC.Types.ChainHead where

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


data ChainHead = ChainHead {
    inProcessList :: Bool,
    head          :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ChainHead where
  parseJSON (Object v) =
    ChainHead <$> v .: "chaininprocesslist" <*> v .: "chainhead"
  parseJSON _ = mzero


instance ToJSON ChainHead where
  toJSON (ChainHead {..}) = object
    [ "chaininprocesslist" .= inProcessList
    , "chainhead" .= head
    ]
  toEncoding (ChainHead {..}) = pairs
    (  "chaininprocesslist"
    .= inProcessList
    <> "chainhead"
    .= head
    )
