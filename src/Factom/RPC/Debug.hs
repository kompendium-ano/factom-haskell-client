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

reqHoldingQueue :: RPC ()
reqHoldingQueue =
  method "holding-queue" None

-- network-info
-- predictive-fer
-- audit-servers
-- federated-servers
-- configuration
-- process-list
-- authorities
-- reload-configuration
-- drop-rate
-- set-drop-rate
-- delay
-- set-delay
-- summary
-- messages
