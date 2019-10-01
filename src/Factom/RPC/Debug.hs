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

module Factom.RPC.Debug where

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

endpoint  = "http://localhost:8088/debug"
endpointRemote = "http://dev.factomd.net/v2/debug"

-- |
--
reqHoldingQueue :: RPC ()
reqHoldingQueue =
  method "holding-queue" None

-- |
--
-- network-info
reqNetworkInfo :: RPC ()
reqNetworkInfo =
  method "network-info" None


-- |
--
-- predictive-fer
reqPredictiveFer :: RPC ()
reqPredictiveFer =
  method "predictive-fer" None


-- |
--
-- audit-servers
reqAuditServers :: RPC ()
reqAuditServers =
  method "audit-servers" None


-- |
--
-- federated-servers
reqFederatedServers :: RPC ()
reqFederatedServers =
  method "federated-servers" None


-- |
--
-- configuration
reqConfiguration :: RPC ()
reqConfiguration =
  method "configuration" None


-- |
--
-- process-list
reqProccessList :: RPC ()
reqProcessList =
  method "process-list" None


-- |
--
-- authorities
reqAuthorities :: RPC ()
reqAuthorities =
  method "authorities" None


-- |
--
-- reload-configuration
reqReloadConfiguration :: RPC ()
reqReloadConfiguration =
  method "reload-configuration" None


-- |
--
-- drop-rate
reqDropRate :: RPC ()
reqDropRate =
  method "drop-rate" None


-- |
--
-- set-drop-rate
reqSetDropRate :: RPC ()
reqSetDropRate =
  method "set-drop-rate" None


-- |
--
-- delay
reqDelay :: RPC ()
reqDelay =
  method "delay" None


-- |
--
-- set-delay
reqSetDelay :: RPC ()
reqSetDelay =
  method "set-delay" None


-- |
--
-- summary
reqSummary :: RPC ()
reqSummary =
  method "summary" None


-- |
--
-- messages
reqMessages :: RPC ()
reqMessages =
  method "messages" None
