{-# LANGUAGE OverloadedStrings #-}

module Mollie.API.Internal where

import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString
import           Data.Monoid
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Network.HTTP.Client  as HTTP
import qualified Network.HTTP.Types   as HTTP

{-|
  Environment to run requests to Mollie with.
-}
data Env = Env
    { env_key     :: Text.Text
    , env_manager :: HTTP.Manager
    }

{-|
  Reader wrapper with Mollie Env.
-}
type Mollie a = Reader.ReaderT Env IO a

endpoint :: Text.Text
endpoint = "https://api.mollie.nl"

version :: Text.Text
version = "v1"

initialRequest :: Text.Text -> Mollie (HTTP.Request)
initialRequest path = do
    api_key <- Reader.asks env_key
    request <- HTTP.parseRequest . Text.unpack $ Text.intercalate "/" [endpoint, version, path]
    return request
        { HTTP.requestHeaders = [ ("Authorization", Text.encodeUtf8 $ "Bearer " <> api_key)
                                , ("Accept", "application/json")
                                ]
        }

execute :: HTTP.Request -> Mollie (Int, ByteString.ByteString)
execute request = do
    manager <- Reader.asks env_manager
    response <- Reader.liftIO $ HTTP.httpLbs request manager
    return (HTTP.statusCode $ HTTP.responseStatus response, HTTP.responseBody response)

send :: (Aeson.ToJSON a) => HTTP.Method -> Text.Text -> a -> Mollie (Int, ByteString.ByteString)
send method url body = do
    request <- initialRequest url
    execute request
        { HTTP.method      = method
        , HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode body
        }

get :: Text.Text -> Mollie (Int, ByteString.ByteString)
get url = do
    request <- initialRequest url
    execute request

delete :: Text.Text -> Mollie (Int, ByteString.ByteString)
delete url = do
    request <- initialRequest url
    execute request
        { HTTP.method = HTTP.methodDelete
        }
