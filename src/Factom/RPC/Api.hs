{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Factom.RPC.Api
    ( runTCPClient
    ) where

import           Control.Exception                (bracket)
import           Control.Remote.Monad.JSON
import           Control.Remote.Monad.JSON.Router (Call, router, transport)
import           Network.Socket                   (HostName, ServiceName,
                                                   SocketType (Stream),
                                                   addrAddress, addrFamily,
                                                   addrProtocol, addrSocketType,
                                                   close, connect, defaultHints,
                                                   getAddrInfo, socket)

import           Factom.RPC.JsonRpc               (JsonRpcT, runJsonRpcT)


--------------------------------------------------------------------------------

endpoint  = "http://localhost:8088/v2"
endpointDaemonRemote = "http://dev.factomd.net/v2"


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
ablockByHeight :: Int -> RPC ()
ablockByHeight height = undefined
  -- method "adblock-by-height" $ List [Int height]

-- | "ack" - Find the status of a transaction
--
ack :: RPC ()
ack = undefined
