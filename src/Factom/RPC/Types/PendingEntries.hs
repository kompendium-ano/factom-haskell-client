{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module PendingEntries where

import           Control.Applicative
import           Control.Monad                  ( forM_
                                                , join
                                                , mzero
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                , decode
                                                , object
                                                , pairs
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Monoid
import           Data.Text                      ( Text )
import qualified GHC.Generics
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data TopLevelElt = TopLevelElt {
    topLevelEltStatus    :: Text,
    topLevelEltChainid   :: Text,
    topLevelEltEntryhash :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevelElt where
  parseJSON (Object v) =
    TopLevelElt <$> v .: "status" <*> v .: "chainid" <*> v .: "entryhash"
  parseJSON _ = mzero


instance ToJSON TopLevelElt where
  toJSON (TopLevelElt {..}) = object
    [ "status" .= topLevelEltStatus
    , "chainid" .= topLevelEltChainid
    , "entryhash" .= topLevelEltEntryhash
    ]
  toEncoding (TopLevelElt {..}) = pairs
    (  "status"
    .= topLevelEltStatus
    <> "chainid"
    .= topLevelEltChainid
    <> "entryhash"
    .= topLevelEltEntryhash
    )


type TopLevel = [TopLevelElt]
