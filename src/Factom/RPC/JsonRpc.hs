{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Factom.RPC.JsonRpc
    ( JsonRpcT (..)
    , Method (..)
    , Request (..)
    , Response (..)
    , Error
    , version
    , runJsonRpcT
    , request
    , mkDefaultRequest
    ) where

import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader     (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.State.Lazy (StateT (..), evalStateT, get,
                                                 modify')
import           Data.Aeson                     (FromJSON (..), ToJSON (..),
                                                 eitherDecode', encode,
                                                 withObject, (.:), (.:?))
import           Data.Aeson.Casing              (snakeCase)
import           Data.Aeson.TH                  (defaultOptions, deriveJSON,
                                                 fieldLabelModifier,
                                                 omitNothingFields)
import           Data.ByteString.Lazy           (fromStrict, toStrict)
import           Data.Either                    (Either)
import           Data.Text                      (Text)
import           Network.Socket                 (Socket)
import           Network.Socket.ByteString      (recv, sendAll)

--------------------------------------------------------------------------------

newtype Version =
  Version Text
    deriving (Eq, Show, ToJSON, FromJSON)

version :: Version
version = Version "2.0"

newtype Method =
  Method Text
    deriving (Eq, Show, ToJSON, FromJSON)

data Request a = Request
    { reqJsonrpc :: Version
    , reqMethod  :: Method
    , reqParams  :: a
    , reqId      :: Int
    } deriving (Show, Eq)

deriveJSON defaultOptions
  { omitNothingFields  = True
  , fieldLabelModifier = snakeCase . drop 3
  } ''Request

mkDefaultRequest :: ToJSON a => Method -> a -> Request a
mkDefaultRequest method req = Request version method req 0

data Error =
  Error
    { code    :: Int
    , message :: Text
    , errData :: Maybe Text
    } deriving (Eq, Show)

deriveJSON defaultOptions ''Error


-- | TODO: handle jsonrpc errors
data Response a =
  Response
    { resJsonrpc :: Version
    , resResult  :: Either Error a
    , resId      :: Int
    } deriving (Show, Eq)


instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "response" $ \o -> do
    v      <- o .: "jsonrpc"
    jid    <- o .: "id"
    result <- o .:? "result"
    e      <- o .:? "error"
    let r = maybe (maybe (fail "invalid response") Left e) Right result
    return $ Response v r jid


unwrapJson :: (Show a, FromJSON a) => Response a -> Either Error a
unwrapJson (Response _ r _) = r


newtype JsonRpcT m a =
  JsonRpcT { unJsonRpcT :: StateT Int (ReaderT Socket m) a }
    deriving ( Functor, Applicative, Monad, MonadIO )


runJsonRpcT :: MonadIO m => Socket -> JsonRpcT m a -> m a
runJsonRpcT s jm = flip runReaderT s . flip evalStateT 0 $ unJsonRpcT jm

request :: (MonadIO m, Show b, ToJSON a, FromJSON b) => Method -> a -> JsonRpcT m (Either Error b)
request method params = do
    s   <- JsonRpcT $ lift ask
    req <- toStrict . encode . Request version method params <$> JsonRpcT get
    liftIO $ sendAll s req
    JsonRpcT $ modify' (+1)
    response <- fromStrict <$> liftIO (recv s 262144)
    either fail (return . unwrapJson) $ eitherDecode' response
