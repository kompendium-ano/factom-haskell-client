{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Factom.RPC.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe
import qualified Data.Text        as T
import           Data.Time
import           GHC.Generics

import           Factom.RPC.Utils
   -- { "jsonrpc": "2.0"
   -- , "id": 0
   -- , "method":"entry"
   -- , "params": {
   --     "hash":"24674e6bc3094eb773297de955ee095a05830e431da13a37382dcdc89d73c7d7"
   --    }
   -- }

--------------------------------------------------------------------------------
data ApiRequest =
  ApiRequest
    { jsonRpc :: T.Text
    , id      :: Int
    , method  :: T.Text
    , params  :: Maybe Object
    }
  deriving (Eq, Show, Generic, ToJSON)

data ApiResponse a =
  ApiResponse
    { jsonRpc :: T.Text
    , id      :: Int
    , result  :: a
    }
  deriving (Eq, Show, Generic, FromJSON)

-- "chainid":"df3ade9eec4b08d5379cc64270c30ea7315d8a8a1a69efe2b98a60ecdd69e604",
-- "content":"...",
-- "extids":[ "466163746f6d416e63686f72436861696e"]
data Entry =
  Entry
    { enChainId :: T.Text
    , enContent :: T.Text
    , enExtIds  :: [T.Text]
    }
  deriving (Eq, Show, Generic, FromJSON)

data EntryBlock =
  EntryBlock
    { blockSequenceNumber :: Int
    }
  deriving (Eq, Show, Generic, FromJSON)

data EntryCreditBalance =
  EntryCreditBalance
    { ecBalance :: Int
    }
  deriving (Eq, Show, Generic, FromJSON)
