{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Mollie.API.Payments
    ( paymentsPath
    , createPayment
    , getPayment
    , getPayments
    -- Re-export relevant types
    , PaymentStatus (..)
    , PaymentMethod (..)
    , PaymentScreenLanguage (..)
    , RecurringType (..)
    , NewPayment (..)
    , PaymentMode (..)
    , PaymentLinks (..)
    , Payment (..)
    , PaymentListLinks (..)
    , PaymentList (..)
    , Failure (..)
    ) where

import qualified Data.Aeson           as Aeson
import           Data.Monoid
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Mollie.API.Internal
import           Mollie.API.Types
import qualified Network.HTTP.Client  as HTTP
import qualified Network.HTTP.Types   as HTTP

{-|
  Payment resource's path, relative to API's versioned url.
-}
paymentsPath :: Text.Text
paymentsPath = "payments"

{-|
  Handler to create a new payment.

  For more info see: https://www.mollie.com/en/docs/reference/payments/create.
-}
createPayment :: NewPayment -> Mollie (Either Failure Payment)
createPayment newPayment = do
    (statusCode, rawBody) <- send HTTP.methodPost path newPayment
    return $ case statusCode of
        201 -> case Aeson.decode rawBody of
            Just payment -> Right payment
            Nothing      -> Left $ ParseFailure rawBody
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = paymentsPath

{-|
  Handler to get a payment by Mollie identifier.

  For more info see: https://www.mollie.com/en/docs/reference/payments/get.
-}
getPayment :: Text.Text -> Mollie (Either Failure Payment)
getPayment identifier = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just payment -> Right payment
            Nothing      -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [paymentsPath, identifier]

{-|
  Handler to get a list of payment. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payments returned with a single call is 250.

  For more info see: https://www.mollie.com/en/docs/reference/payments/list.
-}
getPayments :: Int -> Int -> Mollie (Either Failure PaymentList)
getPayments offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just paymentList -> Right paymentList
            Nothing      -> Left $ ParseFailure rawBody
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = paymentsPath <> query
        query = "?offset=" <> (Text.pack $ show offset) <> "&count=" <> (Text.pack $ show count)
