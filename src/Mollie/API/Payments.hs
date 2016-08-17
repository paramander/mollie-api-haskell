{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Mollie.API.Payments
    ( paymentsPath
    , newPayment
    , createPayment
    , getPayment
    , getPayments
    , newRefund
    , createPaymentRefund
    , getPaymentRefund
    , cancelPaymentRefund
    , getPaymentRefunds
    -- Re-export relevant types
    , PaymentStatus (..)
    , PaymentMethod (..)
    , RecurringType (..)
    , NewPayment (..)
    , Mode (..)
    , PaymentLinks (..)
    , Payment (..)
    , ListLinks (..)
    , List (..)
    , NewRefund (..)
    , RefundStatus (..)
    , Refund (..)
    , Failure (..)
    ) where

import qualified Data.Aeson          as Aeson
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Refunds
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP

{-|
  Payment resource's path, relative to API's versioned url or to a customer resource url.
-}
paymentsPath :: Text.Text
paymentsPath = "payments"

{-|
  Helper to create a minimal new payment.
-}
newPayment :: Double -> Text.Text -> Text.Text -> NewPayment
newPayment amount description redirectUrl = NewPayment
    { newPayment_amount            = amount
    , newPayment_description       = description
    , newPayment_redirectUrl       = redirectUrl
    , newPayment_webhookUrl        = Nothing
    , newPayment_method            = Nothing
    , newPayment_metadata          = Nothing
    , newPayment_locale            = Nothing
    , newPayment_recurringType     = Nothing
    , newPayment_customerId        = Nothing
    , newPayment_issuer            = Nothing
    , newPayment_billingAddress    = Nothing
    , newPayment_billingCity       = Nothing
    , newPayment_billingRegion     = Nothing
    , newPayment_billingPostal     = Nothing
    , newPayment_billingCountry    = Nothing
    , newPayment_shippingAddress   = Nothing
    , newPayment_shippingCity      = Nothing
    , newPayment_shippingRegion    = Nothing
    , newPayment_shippingPostal    = Nothing
    , newPayment_shippingCountry   = Nothing
    , newPayment_billingEmail      = Nothing
    , newPayment_dueDate           = Nothing
    , newPayment_consumerName      = Nothing
    , newPayment_consumerAccount   = Nothing
    , newPayment_customerReference = Nothing
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
        query = "?offset=" <> showT offset <> "&count=" <> showT count

{-|
  Helper to create a minimal new refund. Defaults to refunding the total amount for the targetted payment.
-}
newRefund :: NewRefund
newRefund = NewRefund
    { newRefund_amount      = Nothing
    , newRefund_description = Nothing
    }

{-|
  Handler to create a new refund for a specific payment.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/create.
-}
createPaymentRefund :: Text.Text -> NewRefund -> Mollie (Either Failure Refund)
createPaymentRefund paymentId newRefund = do
    (statusCode, rawBody) <- send HTTP.methodPost path newRefund
    return $ case statusCode of
        201 -> case Aeson.decode rawBody of
            Just refund -> Right refund
            Nothing     -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath]

{-|
  Handler to get a refund by its identifier for a specific payment.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/get.
-}
getPaymentRefund :: Text.Text -> Text.Text -> Mollie (Either Failure Refund)
getPaymentRefund paymentId refundId = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just refund -> Right refund
            Nothing      -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath, refundId]

{-|
  Handler to cancel a refund by its identifier for a specific payment.

  This request only works on refunds which have not yet started processing.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/delete.
-}
cancelPaymentRefund :: Text.Text -> Text.Text -> Mollie (Maybe Failure)
cancelPaymentRefund paymentId refundId = do
    (statusCode, rawBody) <- delete path
    return $ case statusCode of
        204 -> Nothing
        404 -> Just NotFound
        _   -> Just $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath, refundId]

{-|
  Handler to get a list of refunds for a specific payment. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of refunds returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/list.
-}
getPaymentRefunds :: Text.Text -> Int -> Int -> Mollie (Either Failure (List Refund))
getPaymentRefunds paymentId offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just refundList -> Right refundList
            Nothing         -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = (Text.intercalate "/" [paymentsPath, paymentId, refundsPath]) <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
