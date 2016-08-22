{-# LANGUAGE OverloadedStrings #-}

module Mollie.API.Internal where

import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString
import           Data.Monoid
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Mollie.API.Types
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

showT :: (Show a) => a -> Text.Text
showT = Text.pack . show

initialRequest :: Text.Text -> Mollie (HTTP.Request)
initialRequest path = do
    api_key <- Reader.asks env_key
    request <- HTTP.parseRequest . Text.unpack $ Text.intercalate "/" [endpoint, version, path]
    return request
        { HTTP.requestHeaders = [ ("Authorization", Text.encodeUtf8 $ "Bearer " <> api_key)
                                , ("Accept", "application/json")
                                ]
        }

execute :: HTTP.Request -> Mollie (Either ResponseError (Int, ByteString.ByteString))
execute request = do
    manager <- Reader.asks env_manager
    response <- Reader.liftIO $ HTTP.httpLbs request manager
    return $ handleStatus
        (HTTP.statusCode $ HTTP.responseStatus response)
        (HTTP.responseBody response)
    where
        handleStatus status body
            | elem status [200, 201, 204] =
                  Right (status, body)
            | elem status [400, 401, 403, 404, 405, 415, 422, 429] =
                  case Aeson.decode body of
                      Just err -> Left $ ClientError status err
                      Nothing  -> Left UnexpectedResponse
            | elem status [500, 502, 503, 504] =
                  Left $ ServerError status
            | otherwise = Left UnexpectedResponse

send :: (Aeson.ToJSON a)
     => HTTP.Method
     -> Text.Text
     -> a
     -> Mollie (Either ResponseError (Int, ByteString.ByteString))
send method url reqBody = do
    request <- initialRequest url
    execute request
        { HTTP.method      = method
        , HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode reqBody
        }

get :: (Aeson.FromJSON a) => Text.Text -> Mollie (Either ResponseError a)
get url = do
    request <- initialRequest url
    result <- execute request
    return $ decodeResult result

delete :: Text.Text -> Mollie (Either ResponseError (Int, ByteString.ByteString))
delete url = do
    request <- initialRequest url
    execute request
        { HTTP.method = HTTP.methodDelete
        }

ignoreResult :: Either ResponseError (Int, ByteString.ByteString)
             -> Maybe ResponseError
ignoreResult result = case result of
    Right _  -> Nothing
    Left err -> Just err

decodeResult :: (Aeson.FromJSON a)
             => Either ResponseError (Int, ByteString.ByteString)
             -> Either ResponseError a
decodeResult result = case result of
    Right (_, body) ->
        case Aeson.decode body of
            Just resource -> Right resource
            Nothing       -> Left UnexpectedResponse
    Left other -> Left other
