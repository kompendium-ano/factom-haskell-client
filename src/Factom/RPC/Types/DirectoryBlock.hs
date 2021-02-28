{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.RPC.Types.DirectoryBlock where

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

data Header =
  Header
    { headerSequencenumber :: Double
    , headerPrevblockkeymr :: Text
    , headerTimestamp      :: Double
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON Header where
  parseJSON (Object v) =
    Header <$> v .: "sequencenumber" <*> v .: "prevblockkeymr" <*>
    v .: "timestamp"
  parseJSON _ = mzero

instance ToJSON Header where
  toJSON (Header {..}) =
    object
      [ "sequencenumber" .= headerSequencenumber
      , "prevblockkeymr" .= headerPrevblockkeymr
      , "timestamp" .= headerTimestamp
      ]
  toEncoding (Header {..}) =
    pairs
      ("sequencenumber" .= headerSequencenumber <> "prevblockkeymr" .=
       headerPrevblockkeymr <>
       "timestamp" .=
       headerTimestamp)

data EntryblocklistElt =
  EntryblocklistElt
    { entryblocklistEltChainid :: Text
    , entryblocklistEltKeymr   :: Text
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON EntryblocklistElt where
  parseJSON (Object v) = EntryblocklistElt <$> v .: "chainid" <*> v .: "keymr"
  parseJSON _          = mzero

instance ToJSON EntryblocklistElt where
  toJSON (EntryblocklistElt {..}) =
    object
      ["chainid" .= entryblocklistEltChainid, "keymr" .= entryblocklistEltKeymr]
  toEncoding (EntryblocklistElt {..}) =
    pairs
      ("chainid" .= entryblocklistEltChainid <> "keymr" .=
       entryblocklistEltKeymr)

data DirectoryBlock =
  DirectoryBlock
    { topLevelHeader         :: Header
    , topLevelEntryblocklist :: [EntryblocklistElt]
    }
  deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON DirectoryBlock where
  parseJSON (Object v) =
    DirectoryBlock <$> v .: "header" <*> v .: "entryblocklist"
  parseJSON _ = mzero

instance ToJSON DirectoryBlock where
  toJSON (DirectoryBlock {..}) =
    object
      ["header" .= topLevelHeader, "entryblocklist" .= topLevelEntryblocklist]
  toEncoding (DirectoryBlock {..}) =
    pairs
      ("header" .= topLevelHeader <> "entryblocklist" .= topLevelEntryblocklist)
