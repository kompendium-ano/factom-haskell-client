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

module Factom.RPC.Api
    ( runTCPClient
    , reqHeights
    , reqCurrentMinute
    ) where

import           Control.Concurrent
import           Control.Exception                     (bracket)
import           Control.Monad.IO.Class
import           Control.Remote.Monad.JSON
import           Control.Remote.Monad.JSON.Client
import           Control.Remote.Monad.JSON.Router
import           Control.Remote.Monad.JSON.Trace
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Network.Socket                        (HostName, ServiceName,
                                                        SocketType (Stream),
                                                        addrAddress, addrFamily,
                                                        addrProtocol,
                                                        addrSocketType, close,
                                                        connect, defaultHints,
                                                        getAddrInfo, socket)

import           Factom.RPC.JsonRpc                    (JsonRpcT, runJsonRpcT)
import           Factom.RPC.Types.AdminBlock
import           Factom.RPC.Types.DirectoryBlock
import           Factom.RPC.Types.DirectoryBlockHeader
import           Factom.RPC.Types.Heights

--------------------------------------------------------------------------------

endpoint       = "http://51.158.171.20:8088/v2"
endpointRemote = "https://api.factomd.net/v2" -- "http://dev.factomd.net/v2"

runTCPClient :: HostName -> ServiceName -> JsonRpcT IO a -> IO a
runTCPClient host port f = do
  addr <- resolve host port
  bracket (open addr) close talk
    where
      resolve host' port' = do
          let hints = defaultHints { addrSocketType = Stream }
          addr:_ <- getAddrInfo (Just hints) (Just host') (Just port')
          return addr

      open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          return sock

      talk sock = runJsonRpcT sock f

-- | "ablock-by-height" - Retrieve administrative blocks for any given height.
--
reqAblockByHeight :: Int -> RPC ()
reqAblockByHeight height =
  method "adblock-by-height" $ List [toJSON height]

-- | "ack" - Find the status of a transaction
--
reqAck :: RPC ()
reqAck =
  method "ack" None

-- | Get all information about Admin Block
--   based on Merkle Root Tree
reqAdminBlock :: Text -> RPC Ablock
reqAdminBlock mqr =
  method "admin-block" $ List [String mqr]

-- | Retrieve information about the directory block anchors that have been confirmed on Bitcoin and Ethereum.
--
reqAnchors :: RPC ()
reqAnchors =
  method "anchors" None

-- | Retrieve information about the directory block anchors that have been confirmed on Bitcoin and Ethereum.
--
reqChainHead :: RPC ()
reqChainHead =
  method "chain-head" None

-- | Send a Chain Commit Message to factomd to create a new Chain.
--
reqCommitChain :: RPC ()
reqCommitChain =
  method "commit-chain" None

-- | Send an Entry Commit Message to factom to create a new Entry
--
reqCommitEntry :: RPC ()
reqCommitEntry =
  method "commit-entry" None

-- | Return statistic for current call
--
reqCurrentMinute :: RPC ()
reqCurrentMinute =
  method "current-minute" None

-- | Send an Entry Commit Message to factom to create a new Entry
--
reqDBlockByHeight :: RPC ()
reqDBlockByHeight =
  method "dblock-by-height" None

-- | Retrieve basic system information along with a description of the node’s current perception of the network
--
reqDiagnostics :: RPC ()
reqDiagnostics =
  method "diagnostics" None

-- | Get information about directory block
--
reqDirectoryBlock :: Text -> RPC DirectoryBlock
reqDirectoryBlock keymr =
  method "directory-block" $ List [toJSON keymr]

-- | Get the most recently recorded block.
--   The directory block head is the last known directory block by factom.
--   This can be used to grab the latest block and the information required to traverse the entire blockchain.
reqDirectoryBlockHead :: RPC DirectoryBlockHeader
reqDirectoryBlockHead =
  method "directory-block-head" $ None

-- | Retrieve the entry credit block for any given height
--
reqEcblockByHeight :: Int -> RPC ()
reqEcblockByHeight height =
  method "ecblok-by-height" $ List [toJSON height]

-- | Get an Entry from factomd specified by the Entry Hash.
--
reqEntry :: Text -> RPC ()
reqEntry hash =
  method "entry" $ List [String hash]

-- |
--
reqEntryAck :: RPC ()
reqEntryAck =
  method "entry-ack" None

-- |
--
reqEntryBlock :: RPC ()
reqEntryBlock =
  method "entry-block" None

-- |
--
reqEntryCreditBalance :: RPC ()
reqEntryCreditBalance =
  method "entry-credit-balane" None

-- |
--
reqEntryCreditBlock :: RPC ()
reqEntryCreditBlock =
  method "entry-credit-block" None

-- |
--
reqEntryCreditRate :: RPC ()
reqEntryCreditRate =
  method "entry-credit-rate" None

-- |
--
reqFactoidAck :: RPC ()
reqFactoidAck =
  method "factoid-ack" None

-- |
--
reqFactoidBalance :: RPC ()
reqFactoidBalance =
  method "factoid-balance" None

-- |
--
reqFactoidBlock :: RPC ()
reqFactoidBlock =
  method "factoid-blok" None

-- |
--
reqFactoidSubmit :: RPC ()
reqFactoidSubmit =
  method "factoid-submit" None

-- |
--
reqFBlockByHeight :: RPC ()
reqFBlockByHeight =
  method "fblock-by-height" None

-- |
--
reqHeights :: RPC Heights
reqHeights =
  method "heights" None

-- $ Named [("jsonrpc", String "2.0"), ("id", toJSON (0::Int))]


--------------------------------------------------------------------------------

main = do
  let s = weakSession (traceSendAPI "" $ clientSendAPI endpointRemote)
  h <- send s $ do
         h <- reqHeights --ablockByHeight 1000
         return h
  print h
