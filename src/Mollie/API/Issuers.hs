{-# LANGUAGE OverloadedStrings #-}

module Mollie.API.Issuers
    ( issuersPath
    , getIssuer
    , getIssuers
    -- Re-export relevant types
    , PaymentMethod (..)
    , Issuer (..)
    , ListLinks (..)
    , List (..)
    , Failure (..)
    ) where

import qualified Data.Aeson          as Aeson
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Types

{-|
  Issuer resource's path, relative to API's versioned url.
-}
issuersPath :: Text.Text
issuersPath = "issuers"

{-|
  Handler to get an issuer by its identifier.

  For more information see: https://www.mollie.com/en/docs/reference/issuers/get.
-}
getIssuer :: Text.Text -> Mollie (Either Failure Issuer)
getIssuer issuerId = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just issuer -> Right issuer
            Nothing     -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _ -> Left $ RequestFailure statusCode rawBody
    where
        path = (Text.intercalate "/" [issuersPath, issuerId])

{-|
  Handler to get a list of issuers. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payment methods returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/issuers/list.
-}
getIssuers :: Int -> Int -> Mollie (Either Failure (List Issuer))
getIssuers offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just issuerList -> Right issuerList
            Nothing     -> Left $ ParseFailure rawBody
        _ -> Left $ RequestFailure statusCode rawBody
    where
        path = issuersPath <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
