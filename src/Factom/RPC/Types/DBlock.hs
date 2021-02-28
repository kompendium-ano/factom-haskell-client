{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.DBlock where

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

data DbentriesElt =
  DbentriesElt
    { dbentriesEltChainid :: Text
    , dbentriesEltKeymr   :: Text
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON DbentriesElt where
  parseJSON (Object v) = DbentriesElt <$> v .: "chainid" <*> v .: "keymr"
  parseJSON _          = mzero

instance ToJSON DbentriesElt where
  toJSON (DbentriesElt {..}) =
    object ["chainid" .= dbentriesEltChainid, "keymr" .= dbentriesEltKeymr]
  toEncoding (DbentriesElt {..}) =
    pairs ("chainid" .= dbentriesEltChainid <> "keymr" .= dbentriesEltKeymr)

data Header =
  Header
    { headerBodymr       :: Text
    , headerPrevfullhash :: Text
    , headerChainid      :: Text
    , headerNetworkid    :: Double
    , headerDbheight     :: Double
    , headerVersion      :: Double
    , headerBlockcount   :: Double
    , headerTimestamp    :: Double
    , headerPrevkeymr    :: Text
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON Header where
  parseJSON (Object v) =
    Header <$> v .: "bodymr" <*> v .: "prevfullhash" <*> v .: "chainid" <*>
    v .: "networkid" <*>
    v .: "dbheight" <*>
    v .: "version" <*>
    v .: "blockcount" <*>
    v .: "timestamp" <*>
    v .: "prevkeymr"
  parseJSON _ = mzero

instance ToJSON Header where
  toJSON (Header {..}) =
    object
      [ "bodymr" .= headerBodymr
      , "prevfullhash" .= headerPrevfullhash
      , "chainid" .= headerChainid
      , "networkid" .= headerNetworkid
      , "dbheight" .= headerDbheight
      , "version" .= headerVersion
      , "blockcount" .= headerBlockcount
      , "timestamp" .= headerTimestamp
      , "prevkeymr" .= headerPrevkeymr
      ]
  toEncoding (Header {..}) =
    pairs
      ("bodymr" .= headerBodymr <> "prevfullhash" .= headerPrevfullhash <>
       "chainid" .=
       headerChainid <>
       "networkid" .=
       headerNetworkid <>
       "dbheight" .=
       headerDbheight <>
       "version" .=
       headerVersion <>
       "blockcount" .=
       headerBlockcount <>
       "timestamp" .=
       headerTimestamp <>
       "prevkeymr" .=
       headerPrevkeymr)

data Dblock =
  Dblock
    { dblockDbhash    :: Text
    , dblockDbentries :: [DbentriesElt]
    , dblockHeader    :: Header
    , dblockKeymr     :: Text
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON Dblock where
  parseJSON (Object v) =
    Dblock <$> v .: "dbhash" <*> v .: "dbentries" <*> v .: "header" <*>
    v .: "keymr"
  parseJSON _ = mzero

instance ToJSON Dblock where
  toJSON (Dblock {..}) =
    object
      [ "dbhash" .= dblockDbhash
      , "dbentries" .= dblockDbentries
      , "header" .= dblockHeader
      , "keymr" .= dblockKeymr
      ]
  toEncoding (Dblock {..}) =
    pairs
      ("dbhash" .= dblockDbhash <> "dbentries" .= dblockDbentries <> "header" .=
       dblockHeader <>
       "keymr" .=
       dblockKeymr)

data TopLevel =
  TopLevel
    { topLevelRawdata :: Text
    , topLevelDblock  :: Dblock
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .: "rawdata" <*> v .: "dblock"
  parseJSON _          = mzero

instance ToJSON TopLevel where
  toJSON (TopLevel {..}) =
    object ["rawdata" .= topLevelRawdata, "dblock" .= topLevelDblock]
  toEncoding (TopLevel {..}) =
    pairs ("rawdata" .= topLevelRawdata <> "dblock" .= topLevelDblock)
