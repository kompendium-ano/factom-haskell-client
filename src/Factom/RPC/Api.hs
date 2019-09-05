{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Factom.RPC.Api where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.HTTP.Client        (Manager, newManager)
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Conduit       (simpleHttp)
import qualified Network.HTTP.Simple        as S
import           Servant.API
import           Servant.Client

import           Factom.RPC.Types
import           Factom.RPC.Utils

--------------------------------------------------------------------------------

endpoint = "http://localhost:8088/v2"

type FactomAPI =

  -- curl -X POST --data-binary '{"jsonrpc": "2.0", "id": 0, "method":"entry","params":
  -- {"hash":"24674e6bc3094eb773297de955ee095a05830e431da13a37382dcdc89d73c7d7"}}' \
  -- -H 'content-type:text/plain;' http://localhost:8088/v2
       "entry"
    :> ReqBody '[JSON] ApiRequest
    :> Post '[JSON] (ApiResponse Entry)

  -- POST /entry-ack
  :<|> "entry-block"
    :> Post '[JSON] (ApiResponse EntryBlock)

  -- POST /entry-credit-balance
  :<|> "entry-credit-balance"
    :> Post '[JSON] (ApiResponse EntryCreditBalance)

factomAPI :: Proxy FactomAPI
factomAPI = Proxy

-- Derive call functions for the api
getEntry :: ApiRequest -> ClientM (ApiResponse Entry)
getEntryBlock :: ClientM (ApiResponse EntryBlock)
getEntryCreditBalance :: ClientM (ApiResponse EntryCreditBalance)
(     getEntry
 :<|> getEntryBlock
 :<|> getEntryCreditBalance ) = client factomAPI

--------------------------------------------------------------------------------

getEntry' = undefined
getEntryBlock' = undefined
getEntryCreditBalance' = undefined














-- module Factom.Api
--     ( runTCPClient
--     ) where

-- import           Control.Exception (bracket)
-- import           Network.Socket    (HostName, ServiceName, SocketType (Stream),
--                                     addrAddress, addrFamily, addrProtocol,
--                                     addrSocketType, close, connect,
--                                     defaultHints, getAddrInfo, socket)

-- import           Factom.JsonRpc    (JsonRpcT, runJsonRpcT)

-- --------------------------------------------------------------------------------

-- runTCPClient :: HostName -> ServiceName -> JsonRpcT IO a -> IO a
-- runTCPClient host port f = do
--   addr <- resolve host port
--   bracket (open addr) close talk
--     where
--       resolve host' port' = do
--           let hints = defaultHints { addrSocketType = Stream }
--           addr:_ <- getAddrInfo (Just hints) (Just host') (Just port')
--           return addr
--       open addr = do
--           sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--           connect sock $ addrAddress addr
--           return sock
--       talk sock = runJsonRpcT sock f
