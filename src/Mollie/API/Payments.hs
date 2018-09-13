{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Mollie.API.Payments
    ( paymentsPath
    , newPayment
    , newRecurringPayment
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
    , ResponseError (..)
    ) where

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
  Helper to create a minimal new payment for normal use.
-}
newPayment :: Double -- ^ amount
           -> Text.Text -- ^ description
           -> Text.Text -- ^ redirectUrl
           -> NewPayment
newPayment amount description redirectUrl = (newRecurringPayment amount description)
    { newPayment_redirectUrl       = Just redirectUrl
    , newPayment_recurringType     = Nothing
    }

{-|
  Helper to create a minimal new payment for recurring use.

  A payment created with this helper should be sent with the
  `createCustomerPayment` from `Mollie.API.Customers` or have
  the customerId set.

  For a first recurring payment use `newPayment` and set the
  recurring type to `First`, because it needs a return url.
-}
newRecurringPayment :: Double -- ^ amount
                    -> Text.Text -- ^ description
                    -> NewPayment
newRecurringPayment amount description = NewPayment
    { newPayment_amount            = amount
    , newPayment_description       = description
    , newPayment_redirectUrl       = Nothing
    , newPayment_webhookUrl        = Nothing
    , newPayment_method            = Nothing
    , newPayment_metadata          = Nothing
    , newPayment_locale            = Nothing
    , newPayment_recurringType     = Just Recurring
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
createPayment :: NewPayment -> Mollie (Either ResponseError Payment)
createPayment newPayment =
    decodeResult <$> send HTTP.methodPost path newPayment
    where
        path = paymentsPath

{-|
  Handler to get a payment by its identifier.

  For more information see: https://www.mollie.com/en/docs/reference/payments/get.
-}
getPayment :: Text.Text -- ^ paymentId
           -> Mollie (Either ResponseError Payment)
getPayment paymentId = get path
    where
        path = Text.intercalate "/" [paymentsPath, paymentId]

{-|
  Handler to get a list of payment. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payments returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/payments/list.
-}
getPayments :: Int -- ^ offset
            -> Int -- ^ count
            -> Mollie (Either ResponseError (List Payment))
getPayments offset count = get path
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
createPaymentRefund :: Text.Text -- ^ paymentId
                    -> NewRefund
                    -> Mollie (Either ResponseError Refund)
createPaymentRefund paymentId newRefund =
    decodeResult <$> send HTTP.methodPost path newRefund
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath]

{-|
  Handler to get a refund by its identifier for a specific payment.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/get.
-}
getPaymentRefund :: Text.Text -- ^ paymentId
                 -> Text.Text -- ^ refundId
                 -> Mollie (Either ResponseError Refund)
getPaymentRefund paymentId refundId = get path
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath, refundId]

{-|
  Handler to cancel a refund by its identifier for a specific payment.

  This request only works on refunds which have not yet started processing.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/delete.
-}
cancelPaymentRefund :: Text.Text -- ^ paymentId
                    -> Text.Text -- ^ refundId
                    -> Mollie (Maybe ResponseError)
cancelPaymentRefund paymentId refundId =
    ignoreResult <$> delete path
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath, refundId]

{-|
  Handler to get a list of refunds for a specific payment. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of refunds returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/list.
-}
getPaymentRefunds :: Text.Text -- ^ paymentId
                  -> Int -- ^ offset
                  -> Int -- ^ count
                  -> Mollie (Either ResponseError (List Refund))
getPaymentRefunds paymentId offset count = get path
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath] <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
