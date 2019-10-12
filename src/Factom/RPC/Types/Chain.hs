{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Chain where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data LinksElt = LinksElt { 
    linksEltHref :: Text,
    linksEltRel :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON LinksElt where
  parseJSON (Object v) = LinksElt <$> v .:   "href" <*> v .:   "rel"
  parseJSON _          = mzero


instance ToJSON LinksElt where
  toJSON     (LinksElt {..}) = object ["href" .= linksEltHref, "rel" .= linksEltRel]
  toEncoding (LinksElt {..}) = pairs  ("href" .= linksEltHref<>"rel" .= linksEltRel)


data Result = Result { 
    resultStatus :: Text,
    resultCreatedAt :: Text,
    resultChainId :: Text,
    resultExtIds :: [Text],
    resultLinks :: [LinksElt],
    resultSynced :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Result where
  parseJSON (Object v) = Result <$> v .:   "status" <*> v .:   "createdAt" <*> v .:   "chainId" <*> v .:   "extIds" <*> v .:   "links" <*> v .:   "synced"
  parseJSON _          = mzero


instance ToJSON Result where
  toJSON     (Result {..}) = object ["status" .= resultStatus, "createdAt" .= resultCreatedAt, "chainId" .= resultChainId, "extIds" .= resultExtIds, "links" .= resultLinks, "synced" .= resultSynced]
  toEncoding (Result {..}) = pairs  ("status" .= resultStatus<>"createdAt" .= resultCreatedAt<>"chainId" .= resultChainId<>"extIds" .= resultExtIds<>"links" .= resultLinks<>"synced" .= resultSynced)


data TopLevel = TopLevel { 
    topLevelResult :: Result
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "result"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["result" .= topLevelResult]
  toEncoding (TopLevel {..}) = pairs  ("result" .= topLevelResult)




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


