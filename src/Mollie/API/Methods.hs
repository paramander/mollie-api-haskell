{-# LANGUAGE OverloadedStrings #-}

module Mollie.API.Methods
    ( methodsPath
    , getMethod
    , getMethods
    -- Re-export relevant types
    , PaymentMethod (..)
    , Locale (..)
    , MethodAmount (..)
    , MethodImage (..)
    , Method (..)
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
  Payment method resource's path, relative to API's versioned url.
-}
methodsPath :: Text.Text
methodsPath = "methods"

{-|
  Handler to get a payment method by its identifier.

  Fails on payment methods which are not enabled.

  For more information see: https://www.mollie.com/en/docs/reference/methods/get.
-}
getMethod :: PaymentMethod -> Locale -> Mollie (Either Failure Method)
getMethod methodId locale = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just method -> Right method
            Nothing     -> Left $ ParseFailure rawBody
        _ -> Left $ RequestFailure statusCode rawBody
    where
        path = (Text.intercalate "/" [methodsPath, showT methodId]) <> query
        query = "?locale=" <> showT locale

{-|
  Handler to get a list of payment methods. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payment methods returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/methods/list.
-}
getMethods :: Locale -> Int -> Int -> Mollie (Either Failure (List Method))
getMethods locale offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just refundList -> Right refundList
            Nothing         -> Left $ ParseFailure rawBody
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = methodsPath <> query
        query = "?locale=" <> showT locale <> "&offset=" <> showT offset <> "&count=" <> showT count
