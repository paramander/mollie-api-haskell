{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Mollie.API.Payments
    ( paymentsPath
    , newPayment
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
    , ListLinks (..)
    , List (..)
    , Failure (..)
    ) where

import qualified Data.Aeson          as Aeson
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP

{-|
  Payment resource's path, relative to API's versioned url.
-}
paymentsPath :: Text.Text
paymentsPath = "payments"

{-|
  Helper to create a minimal new payment.
-}
newPayment :: Double -> Text.Text -> Text.Text -> NewPayment
newPayment amount description redirectUrl = NewPayment
    { newPayment_amount        = amount
    , newPayment_description   = description
    , newPayment_redirectUrl   = redirectUrl
    , newPayment_webhookUrl    = Nothing
    , newPayment_method        = Nothing
    , newPayment_metadata      = Nothing
    , newPayment_locale        = Nothing
    , newPayment_recurringType = Nothing
    }

{-|
  Handler to create a new payment.

  For more information see: https://www.mollie.com/en/docs/reference/payments/create.
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
  Handler to get a payment by its identifier.

  For more information see: https://www.mollie.com/en/docs/reference/payments/get.
-}
getPayment :: Text.Text -> Mollie (Either Failure Payment)
getPayment paymentId = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just payment -> Right payment
            Nothing      -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [paymentsPath, paymentId]

{-|
  Handler to get a list of payment. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payments returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/payments/list.
-}
getPayments :: Int -> Int -> Mollie (Either Failure (List Payment))
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
