{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module JsonDataEcBlok where

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


data EntriesElt = EntriesElt {
    entriesEltEntryhash :: (Maybe (Text:|:[(Maybe Value)])),
    entriesEltCredits :: (Maybe (Double:|:[(Maybe Value)])),
    entriesEltVersion :: (Maybe (Double:|:[(Maybe Value)])),
    entriesEltSig :: (Maybe (Text:|:[(Maybe Value)])),
    entriesEltServerindexnumber :: (Maybe (Double:|:[(Maybe Value)])),
    entriesEltMillitime :: (Maybe (Text:|:[(Maybe Value)])),
    entriesEltNumber :: (Maybe (Double:|:[(Maybe Value)])),
    entriesEltEcpubkey :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON EntriesElt where
  parseJSON (Object v) =
    EntriesElt
      <$>  v
      .:?? "entryhash"
      <*>  v
      .:?? "credits"
      <*>  v
      .:?? "version"
      <*>  v
      .:?? "sig"
      <*>  v
      .:?? "serverindexnumber"
      <*>  v
      .:?? "millitime"
      <*>  v
      .:?? "number"
      <*>  v
      .:?? "ecpubkey"
  parseJSON _ = mzero


instance ToJSON EntriesElt where
  toJSON (EntriesElt {..}) = object
    [ "entryhash" .= entriesEltEntryhash
    , "credits" .= entriesEltCredits
    , "version" .= entriesEltVersion
    , "sig" .= entriesEltSig
    , "serverindexnumber" .= entriesEltServerindexnumber
    , "millitime" .= entriesEltMillitime
    , "number" .= entriesEltNumber
    , "ecpubkey" .= entriesEltEcpubkey
    ]
  toEncoding (EntriesElt {..}) = pairs
    (  "entryhash"
    .= entriesEltEntryhash
    <> "credits"
    .= entriesEltCredits
    <> "version"
    .= entriesEltVersion
    <> "sig"
    .= entriesEltSig
    <> "serverindexnumber"
    .= entriesEltServerindexnumber
    <> "millitime"
    .= entriesEltMillitime
    <> "number"
    .= entriesEltNumber
    <> "ecpubkey"
    .= entriesEltEcpubkey
    )


data Body = Body {
    bodyEntries :: [EntriesElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Body where
  parseJSON (Object v) = Body <$> v .: "entries"
  parseJSON _          = mzero


instance ToJSON Body where
  toJSON (Body {..}) = object ["entries" .= bodyEntries]
  toEncoding (Body {..}) = pairs ("entries" .= bodyEntries)


data Header = Header {
    headerPrevheaderhash :: Text,
    headerBodyhash :: Text,
    headerBodysize :: Double,
    headerPrevfullhash :: Text,
    headerChainid :: Text,
    headerEcchainid :: Text,
    headerDbheight :: Double,
    headerHeaderexpansionarea :: Text,
    headerObjectcount :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Header where
  parseJSON (Object v) =
    Header
      <$> v
      .:  "prevheaderhash"
      <*> v
      .:  "bodyhash"
      <*> v
      .:  "bodysize"
      <*> v
      .:  "prevfullhash"
      <*> v
      .:  "chainid"
      <*> v
      .:  "ecchainid"
      <*> v
      .:  "dbheight"
      <*> v
      .:  "headerexpansionarea"
      <*> v
      .:  "objectcount"
  parseJSON _ = mzero


instance ToJSON Header where
  toJSON (Header {..}) = object
    [ "prevheaderhash" .= headerPrevheaderhash
    , "bodyhash" .= headerBodyhash
    , "bodysize" .= headerBodysize
    , "prevfullhash" .= headerPrevfullhash
    , "chainid" .= headerChainid
    , "ecchainid" .= headerEcchainid
    , "dbheight" .= headerDbheight
    , "headerexpansionarea" .= headerHeaderexpansionarea
    , "objectcount" .= headerObjectcount
    ]
  toEncoding (Header {..}) = pairs
    (  "prevheaderhash"
    .= headerPrevheaderhash
    <> "bodyhash"
    .= headerBodyhash
    <> "bodysize"
    .= headerBodysize
    <> "prevfullhash"
    .= headerPrevfullhash
    <> "chainid"
    .= headerChainid
    <> "ecchainid"
    .= headerEcchainid
    <> "dbheight"
    .= headerDbheight
    <> "headerexpansionarea"
    .= headerHeaderexpansionarea
    <> "objectcount"
    .= headerObjectcount
    )


data Ecblock = Ecblock {
    ecblockBody :: Body,
    ecblockHeader :: Header
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Ecblock where
  parseJSON (Object v) = Ecblock <$> v .: "body" <*> v .: "header"
  parseJSON _          = mzero


instance ToJSON Ecblock where
  toJSON (Ecblock {..}) =
    object ["body" .= ecblockBody, "header" .= ecblockHeader]
  toEncoding (Ecblock {..}) =
    pairs ("body" .= ecblockBody <> "header" .= ecblockHeader)


data TopLevel = TopLevel {
    topLevelRawdata :: Text,
    topLevelEcblock :: Ecblock
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .: "rawdata" <*> v .: "ecblock"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) =
    object ["rawdata" .= topLevelRawdata, "ecblock" .= topLevelEcblock]
  toEncoding (TopLevel {..}) =
    pairs ("rawdata" .= topLevelRawdata <> "ecblock" .= topLevelEcblock)




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


