{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Factom.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe
import qualified Data.Text     as T
import           Data.Time
import           GHC.Generics

import           Factom.Utils

--------------------------------------------------------------------------------

data ApiResponse a =
  ApiResponse
   { jsonRpc :: T.Text
   , id      :: Int
   , result  :: a
   } deriving (Eq, Show, Generic, FromJSON)

-- "chainid":"df3ade9eec4b08d5379cc64270c30ea7315d8a8a1a69efe2b98a60ecdd69e604",
-- "content":"...",
-- "extids":[ "466163746f6d416e63686f72436861696e"]
data Entry =
  Entry
    { enChainId :: T.Text
    , enContent :: T.Text
    , enExtIds  :: [T.Text]
    } deriving (Eq, Show, Generic, FromJSON)

data EntryBlock =
  EntryBlock
   { blockSequenceNumber :: Int
   } deriving (Eq, Show, Generic, FromJSON)

data EntryCreditBalance =
  EntryCreditBalance
    { ecBalance :: Int
    } deriving (Eq, Show, Generic, FromJSON)
