{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Mollie.API.Internal where

import qualified Data.Aeson                as Aeson
import qualified Data.ByteString           as ByteString hiding (pack)
import qualified Data.ByteString.Char8     as ByteString
import qualified Data.ByteString.Lazy      as LazyByteString
import           Data.Monoid
import qualified Data.Text                 as Text
import           Data.Typeable             (Typeable)
import           Mollie.API.Types
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as HTTP
import           Network.HTTP.Media        ((//))
import qualified Network.HTTP.Types        as HTTP
import           Network.HTTP.Types.Header
import           Servant.API
import           Servant.API.ContentTypes  (eitherDecodeLenient)
import           Servant.Client

{-|
  Mollie returns all API calls with "Content-Type: application/hal+json"
-}
data HalJSON deriving Typeable

instance Accept HalJSON where
    contentType _ = "application" // "hal+json"

instance Aeson.ToJSON a => MimeRender HalJSON a where
    mimeRender _ = Aeson.encode

instance Aeson.FromJSON a => MimeUnrender HalJSON a where
    mimeUnrender _ = eitherDecodeLenient

instance ToHttpApiData PaymentMethod where
    toUrlPiece a = toText a

handleError :: ServantError -> ResponseError
handleError failure =
    case failure of
        FailureResponse response ->
            servantResponseToError (HTTP.statusCode $ responseStatusCode response) (responseBody response)
        DecodeFailure expectedType response ->
            UnexpectedResponse expectedType
        UnsupportedContentType mediaType response ->
            UnexpectedResponse (Text.pack $ "Unsupported media type " ++ show mediaType)
        InvalidContentTypeHeader response ->
            UnexpectedResponse (Text.pack "Invalid content type header")
        ConnectionError explanation ->
            UnexpectedResponse explanation

servantResponseToError :: Int -- ^ _status
                       -> LazyByteString.ByteString -- ^ body
                       -> ResponseError
servantResponseToError _status _body
    | elem _status [400, 401, 403, 404, 405, 415, 422, 429] =
      case Aeson.eitherDecode _body of
          Right err          -> ClientError _status err
          Left decodeFailure -> UnexpectedResponse (Text.pack decodeFailure)
    | elem _status [500, 502, 503, 504] =
          ServerError _status
    | otherwise = UnexpectedResponse (Text.pack "Unhandled status code")

{-|
  Setup the environment for executing API calls
-}
createEnv :: String -- ^ mollieApiKey
          -> IO ClientEnv
createEnv mollieApiKey = do
    let _settings = HTTP.tlsManagerSettings { HTTP.managerModifyRequest = applyMollieHeaders mollieApiKey }
    _manager <- HTTP.newManager _settings
    _baseUrl <- parseBaseUrl "https://api.mollie.com"
    return $ mkClientEnv _manager _baseUrl

applyMollieHeaders :: String -> HTTP.Request -> IO HTTP.Request
applyMollieHeaders key req = return $ setHeader HTTP.hAuthorization (ByteString.pack $ "Bearer " ++ key) req

-- | Set the request headers.
setHeaders :: RequestHeaders -> HTTP.Request -> HTTP.Request
setHeaders hs req = req { HTTP.requestHeaders = hs }

-- | Set the request header by name, removing any other headers with the same
-- name.
setHeader :: HeaderName -> ByteString.ByteString -> HTTP.Request -> HTTP.Request
setHeader n v req =
  setHeaders (filter ((/= n) . fst) (HTTP.requestHeaders req) ++ [(n, v)]) req

-- | Add headers to the request.
addHeaders :: RequestHeaders -> HTTP.Request -> HTTP.Request
addHeaders hs req = setHeaders (HTTP.requestHeaders req ++ hs) req

-- | Add a single header.
addHeader :: HeaderName -> ByteString.ByteString -> HTTP.Request -> HTTP.Request
addHeader n v = addHeaders [(n, v)]
