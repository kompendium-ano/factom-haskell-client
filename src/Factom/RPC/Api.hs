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
    ) where

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
import           Factom.RPC.Types.AdminBlock

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



-- |
--
reqHeights :: RPC ()
reqHeights =
  method "heights" None -- $ Named [("jsonrpc", String "2.0"), ("id", toJSON (0::Int))]


-------------------------

main = do
  let s = strongSession (traceSendAPI "" $ clientSendAPI endpointDaemonRemote)
  h <- send s $ do
         h <- reqHeights --ablockByHeight 1000
         return h
  print h
