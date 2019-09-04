{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever)
import qualified Data.Text                   as T
import           Data.Time.Clock
import           System.Environment

import qualified Factom.Api                  as FCTApi
import qualified Factom.Types                as FCTApi

--------------------------------------------------------------------------------

main :: IO ()
main =
  do
    let delay = 60000  -- 1 request per minute available
    a       <- getArgs
    timeVar <- newTVarIO (delay*1000)
    case a of
      ["-t"]    -> do
        -- read token from file
        return ()
      otherwise -> do
        putStrLn $ "Factom | Establishing connection"
        putStrLn $ "Factom | Getting last entries\n"
        --mentries <- MBApi.getEntries
        --putStrLn $ show $ mcurrs
        -- mapM_ FCTApi.showEntryBlock mentries
        return ()
