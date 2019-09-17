{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module JsonDataDirectoryBlock where

import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import qualified Data.ByteString.Lazy.Char8    as BSL
import           System.Environment             ( getArgs )
import           Control.Monad                  ( forM_
                                                , mzero
                                                , join
                                                )
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson                     ( decode
                                                , Value(..)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , pairs
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                , object
                                                )
import           Data.Monoid
import           Data.Text                      ( Text )
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data Header = Header {
    headerSequencenumber :: Double,
    headerPrevblockkeymr :: Text,
    headerTimestamp :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Header where
  parseJSON (Object v) =
    Header
      <$> v
      .:  "sequencenumber"
      <*> v
      .:  "prevblockkeymr"
      <*> v
      .:  "timestamp"
  parseJSON _ = mzero


instance ToJSON Header where
  toJSON (Header {..}) = object
    [ "sequencenumber" .= headerSequencenumber
    , "prevblockkeymr" .= headerPrevblockkeymr
    , "timestamp" .= headerTimestamp
    ]
  toEncoding (Header {..}) = pairs
    (  "sequencenumber"
    .= headerSequencenumber
    <> "prevblockkeymr"
    .= headerPrevblockkeymr
    <> "timestamp"
    .= headerTimestamp
    )


data EntryblocklistElt = EntryblocklistElt {
    entryblocklistEltChainid :: Text,
    entryblocklistEltKeymr :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON EntryblocklistElt where
  parseJSON (Object v) = EntryblocklistElt <$> v .: "chainid" <*> v .: "keymr"
  parseJSON _          = mzero


instance ToJSON EntryblocklistElt where
  toJSON (EntryblocklistElt {..}) = object
    ["chainid" .= entryblocklistEltChainid, "keymr" .= entryblocklistEltKeymr]
  toEncoding (EntryblocklistElt {..}) = pairs
    ("chainid" .= entryblocklistEltChainid <> "keymr" .= entryblocklistEltKeymr)


data TopLevel = TopLevel {
    topLevelHeader :: Header,
    topLevelEntryblocklist :: [EntryblocklistElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .: "header" <*> v .: "entryblocklist"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) = object
    ["header" .= topLevelHeader, "entryblocklist" .= topLevelEntryblocklist]
  toEncoding (TopLevel {..}) = pairs
    ("header" .= topLevelHeader <> "entryblocklist" .= topLevelEntryblocklist)




parse :: FilePath -> IO TopLevel
parse filename = do
  input <- BSL.readFile filename
  case decode input of
    Nothing -> fatal $ case (decode input :: Maybe Value) of
      Nothing -> "Invalid JSON file: " ++ filename
      Just v  -> "Mismatched JSON value from file: " ++ filename
    Just r -> return (r :: TopLevel)
 where
  fatal :: String -> IO a
  fatal msg = do
    hPutStrLn stderr msg
    exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_
    filenames
    (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


