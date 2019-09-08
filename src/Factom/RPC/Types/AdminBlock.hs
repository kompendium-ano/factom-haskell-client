{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.AdminBlock where

import           Control.Applicative
import           Control.Monad                   (forM_, join, mzero)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..), decode, object,
                                                  pairs, (.:), (.:?), (.=))
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Monoid
import           Data.Text                       (Text)
import qualified GHC.Generics
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)


--------------------------------------------------------------------------------


-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data Header = Header {
    headerBodysize            :: Double,
    headerMessagecount        :: Double,
    headerChainid             :: Text,
    headerHeaderexpansionsize :: Double,
    headerDbheight            :: Double,
    headerHeaderexpansionarea :: Text,
    headerAdminchainid        :: Text,
    headerPrevbackrefhash     :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Header where
  parseJSON (Object v) = Header <$> v .:   "bodysize" <*> v .:   "messagecount" <*> v .:   "chainid" <*> v .:   "headerexpansionsize" <*> v .:   "dbheight" <*> v .:   "headerexpansionarea" <*> v .:   "adminchainid" <*> v .:   "prevbackrefhash"
  parseJSON _          = mzero


instance ToJSON Header where
  toJSON     (Header {..}) = object ["bodysize" .= headerBodysize, "messagecount" .= headerMessagecount, "chainid" .= headerChainid, "headerexpansionsize" .= headerHeaderexpansionsize, "dbheight" .= headerDbheight, "headerexpansionarea" .= headerHeaderexpansionarea, "adminchainid" .= headerAdminchainid, "prevbackrefhash" .= headerPrevbackrefhash]
  toEncoding (Header {..}) = pairs  ("bodysize" .= headerBodysize<>"messagecount" .= headerMessagecount<>"chainid" .= headerChainid<>"headerexpansionsize" .= headerHeaderexpansionsize<>"dbheight" .= headerDbheight<>"headerexpansionarea" .= headerHeaderexpansionarea<>"adminchainid" .= headerAdminchainid<>"prevbackrefhash" .= headerPrevbackrefhash)


data Prevdbsig = Prevdbsig {
    prevdbsigPub :: Text,
    prevdbsigSig :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Prevdbsig where
  parseJSON (Object v) = Prevdbsig <$> v .:   "pub" <*> v .:   "sig"
  parseJSON _          = mzero


instance ToJSON Prevdbsig where
  toJSON     (Prevdbsig {..}) = object ["pub" .= prevdbsigPub, "sig" .= prevdbsigSig]
  toEncoding (Prevdbsig {..}) = pairs  ("pub" .= prevdbsigPub<>"sig" .= prevdbsigSig)


data AbentriesElt = AbentriesElt {
    abentriesEltPrevdbsig            :: (Maybe (Prevdbsig:|:[(Maybe Value)])),
    abentriesEltMinutenumber         :: (Maybe (Double:|:[(Maybe Value)])),
    abentriesEltIdentityadminchainid :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON AbentriesElt where
  parseJSON (Object v) = AbentriesElt <$> v .:?? "prevdbsig" <*> v .:?? "minutenumber" <*> v .:?? "identityadminchainid"
  parseJSON _          = mzero


instance ToJSON AbentriesElt where
  toJSON     (AbentriesElt {..}) = object ["prevdbsig" .= abentriesEltPrevdbsig, "minutenumber" .= abentriesEltMinutenumber, "identityadminchainid" .= abentriesEltIdentityadminchainid]
  toEncoding (AbentriesElt {..}) = pairs  ("prevdbsig" .= abentriesEltPrevdbsig<>"minutenumber" .= abentriesEltMinutenumber<>"identityadminchainid" .= abentriesEltIdentityadminchainid)


data Ablock = Ablock {
    ablockBackreferencehash :: Text,
    ablockHeader            :: Header,
    ablockLookuphash        :: Text,
    ablockAbentries         :: [AbentriesElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Ablock where
  parseJSON (Object v) = Ablock <$> v .:   "backreferencehash" <*> v .:   "header" <*> v .:   "lookuphash" <*> v .:   "abentries"
  parseJSON _          = mzero


instance ToJSON Ablock where
  toJSON     (Ablock {..}) = object ["backreferencehash" .= ablockBackreferencehash, "header" .= ablockHeader, "lookuphash" .= ablockLookuphash, "abentries" .= ablockAbentries]
  toEncoding (Ablock {..}) = pairs  ("backreferencehash" .= ablockBackreferencehash<>"header" .= ablockHeader<>"lookuphash" .= ablockLookuphash<>"abentries" .= ablockAbentries)


data Result = Result {
    resultRawdata :: Text,
    resultAblock  :: Ablock
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Result where
  parseJSON (Object v) = Result <$> v .:   "rawdata" <*> v .:   "ablock"
  parseJSON _          = mzero


instance ToJSON Result where
  toJSON     (Result {..}) = object ["rawdata" .= resultRawdata, "ablock" .= resultAblock]
  toEncoding (Result {..}) = pairs  ("rawdata" .= resultRawdata<>"ablock" .= resultAblock)


data TopLevel = TopLevel {
    topLevelResult  :: Result,
    topLevelJsonrpc :: Text,
    topLevelId      :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "result" <*> v .:   "jsonrpc" <*> v .:   "id"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["result" .= topLevelResult, "jsonrpc" .= topLevelJsonrpc, "id" .= topLevelId]
  toEncoding (TopLevel {..}) = pairs  ("result" .= topLevelResult<>"jsonrpc" .= topLevelJsonrpc<>"id" .= topLevelId)




parse :: FilePath -> IO TopLevel
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
