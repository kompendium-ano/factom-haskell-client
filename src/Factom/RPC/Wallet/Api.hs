{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Factom.RPC.Wallet.Api where

import           Control.Concurrent
import           Control.Exception                (bracket)
import           Control.Monad.IO.Class
import           Control.Remote.Monad.JSON
import           Control.Remote.Monad.JSON.Client
import           Control.Remote.Monad.JSON.Router
import           Control.Remote.Monad.JSON.Trace
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Network.Socket                   (HostName, ServiceName,
                                                   SocketType (Stream),
                                                   addrAddress, addrFamily,
                                                   addrProtocol, addrSocketType,
                                                   close, connect, defaultHints,
                                                   getAddrInfo, socket)

import           Factom.RPC.JsonRpc               (JsonRpcT, runJsonRpcT)

--------------------------------------------------------------------------------

endpoint = "http://localhost:8089/v2"

-- active-identity-keys
-- add-ec-output
-- add-fee
-- add-input
-- add-output
-- address
-- all-addresses
-- all-identity-keys
-- compose-chain
-- compose-entry
-- compose-identity-attribute
-- compose-identity-attribute-endorsement
-- compose-identity-chain
-- compose-identity-key-replacement
-- compose-transaction
-- delete-transaction
-- generate-ec-address
-- generate-factoid-address
-- generate-identity-key
-- get-height
-- identity-key
-- import-addresses
-- import-identity-keys
-- import-koinify
-- new-transaction
-- properties
-- remove-address
-- remove-identity-key
-- sign-data
-- sign-transaction
-- sub-fee
-- tmp-transactions
-- transactions (Retrieving)
-- unlock-wallet
-- wallet-backup
-- wallet-balances
