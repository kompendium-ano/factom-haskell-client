{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever)
import qualified Data.Text                   as T
import           Data.Time.Clock
import           System.Environment

import qualified Factom.RPC.Api              as FApi
import qualified Factom.RPC.Types            as FApi

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let delay = 60000 -- 1 request per minute available
  a <- getArgs
  timeVar <- newTVarIO (delay * 1000)
  case a of
    ["-t"]
        -- read token from file
     -> do
      return ()
    otherwise -> do
      putStrLn $ "Factom | Establishing connection"
      putStrLn $ "Factom | Getting last entries\n"
        --mentries <- FApi.getEntries
        --putStrLn $ show $ mcurrs
        -- mapM_ FApi.showEntryBlock mentries
      return ()
