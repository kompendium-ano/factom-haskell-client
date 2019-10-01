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
reqActiveIdentityKeys :: RPC ()
reqActiveIdentityKeys =
  method "active-identity-keys" None

-- add-ec-output
reqAddEcOutput :: RPC ()
reqAddEcOutput =
  method "add-ec-output" None

-- add-fee
reqAddFee :: RPC ()
reqAddFee =
  method "add-fee" None

-- add-input
reqAddInput :: RPC ()
reqAddInput =
  method "add-input" None

-- add-output
reqAddOutput :: RPC ()
reqAddOutput =
  method "add-output" None

-- address
reqAddress :: RPC ()
reqAddress =
  method "address" None

-- all-addresses
reqAllAddresses :: RPC ()
reqAllAddresses =
  method "all-addresses" None

-- all-identity-keys
reqAllIdentityKeys :: RPC ()
reqAllIdentityKeys =
  method "all-identity-keys" None

-- compose-chain
reqComposeChain :: RPC ()
reqComposeChain =
  method "compose-chain" None

-- compose-entry
reqComposeEntry :: RPC ()
reqComposeEntry =
  method "compose-entry" None

-- compose-identity-attribute
reqComposeIdentityAttribute :: RPC ()
reqComposeIdentityAttribute =
  method "compose-identity-attribute" None

-- compose-identity-attribute-endorsement
reqComposeIdentityAttributeEndorsement :: RPC ()
reqComposeIdentityAttributeEndorsement =
  method "compose-identity-attribute-endorsement" None

-- compose-identity-chain
reqComposeIdentityChain :: RPC ()
reqComposeIdentityChain =
  method "compose-identity-chain" None

-- compose-identity-key-replacement
reqComposeIdentityChainReplacement :: RPC ()
reqComposeIdentityChainReplacement =
  method "compose-identity-key-replacement" None

-- compose-transaction
reqComposeTransaction :: RPC ()
reqComposeTransaction =
  method "compose-transaction" None

-- delete-transaction
reqDeleteTransaction :: RPC ()
reqDeleteTransaction =
  method "delete-transaction" None

-- generate-ec-address
reqGenerateEcAddress :: RPC ()
reqGenerateEcAddress =
  method "generate-ec-address" None

-- generate-factoid-address
reqGenerateFactoidAddress :: RPC ()
reqGenerateFactoidAddress =
  method "generate-factoid-address" None

-- generate-identity-key
reqGenerateIdentityKey :: RPC ()
reqGenerateIdentityKey =
  method "generate-identity-key" None

-- get-height
reqGetHeight :: RPC ()
reqGetHeight =
  method "get-height" None

-- identity-key
reqIdentityKey :: RPC ()
reqIdentityKey =
  method "identity-key" None

-- import-addresses
reqImportAddresses :: RPC ()
reqImportAddresses =
  method "import-addresses" None

-- import-identity-keys
reqImportIdentityKeys :: RPC ()
reqImportIdentityKeys =
  method "import-identity-keys" None

-- import-koinify
reqImportKoinify :: RPC ()
reqImportKoinify =
  method "import-koinify" None

-- new-transaction
reqNewTransaction :: RPC ()
reqNewTransaction =
  method "new-transaction" None

-- properties
reqPropertiess :: RPC ()
reqPropertiess =
  method "properties" None

-- remove-address
reqRemoveAddress :: RPC ()
reqRemoveAddress =
  method "remove-address" None

-- remove-identity-key
reqRemoveIdentityKey :: RPC ()
reqRemoveIdentityKey =
  method "remove-identity-key" None

-- sign-data
reqSignData :: RPC ()
reqSignData =
  method "sign-data" None

-- sign-transaction
reqSignTransaction :: RPC ()
reqSignTransaction =
  method "sign-transaction" None

-- sub-fee
reqSubFee :: RPC ()
reqSubFee =
  method "sub-fee" None

-- tmp-transactions
reqTmpTransactions :: RPC ()
reqTmpTransactions =
  method "tmp-transactions" None

-- transactions (Retrieving)
reqRetrievingTransactions :: RPC ()
reqRetrievingTransactions =
  method "transactions (Retrieving)" None

-- unlock-wallet
reqUnlockWallet :: RPC ()
reqUnlockWallet =
  method "unlock-wallet" None

-- wallet-backup
reqWalletBackup :: RPC ()
reqWalletBackup =
  method "wallet-backup" None

-- wallet-balances
reqWalletBalances :: RPC ()
reqWalletBalances =
  method "wallet-balances" None
