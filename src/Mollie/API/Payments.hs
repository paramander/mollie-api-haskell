{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

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
    , SequenceType (..)
    , NewPayment (..)
    , Payment (..)
    -- Lens getters
    , Mollie.API.Payments.id
    , mode
    , createdAt
    , status
    , isCancelable
    , paidAt
    , canceledAt
    , expiredAt
    , failedAt
    , amount
    , amountRefunded
    , amountRemaining
    , description
    , redirectUrl
    , webhookUrl
    , method
    , metadata
    , locale
    , countryCode
    , profileId
    , settlementAmount
    , settlementId
    , customerId
    , sequenceType
    , mandateId
    , subscriptionId
    , details
    , issuer
    , billingAddress
    , shippingAddress
    , billingEmail
    , dueDate
    , consumerName
    , consumerAccount
    , customerReference
    ) where

import           Control.Lens        (makeFieldsNoPrefix, (&), (.~))
import           Data.Default        (def)
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import qualified Mollie.API.Refunds  as Refunds
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP

makeFieldsNoPrefix ''NewPayment
makeFieldsNoPrefix ''Payment

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
newPayment _amount _description _redirectUrl =
    (newRecurringPayment _amount _description)
      & redirectUrl .~ Just _redirectUrl
      & sequenceType .~ Just Oneoff

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
newRecurringPayment _amount _description =
    def
      & amount .~ (defaultAmount _amount)
      & description .~ _description
      & redirectUrl .~ Nothing
      & webhookUrl .~ Nothing
      & method .~ Nothing
      & metadata .~ Nothing
      & locale .~ Nothing
      & sequenceType .~ Just Recurring
      & customerId .~ Nothing
      & mandateId .~ Nothing
      & issuer .~ Nothing
      & billingAddress .~ Nothing
      & shippingAddress .~ Nothing
      & billingEmail .~ Nothing
      & dueDate .~ Nothing
      & consumerName .~ Nothing
      & consumerAccount .~ Nothing
      & customerReference .~ Nothing

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
getPayments :: Int -- ^ from
            -> Int -- ^ limit
            -> Mollie (Either ResponseError (List Payment))
getPayments from limit = get path
    where
        path = paymentsPath <> query
        query = "?from=" <> showT from <> "&limit=" <> showT limit

{-|
  Helper to create a minimal new refund. Defaults to refunding the total amount for the targetted payment.
-}
newRefund :: NewRefund
newRefund = def

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
        path = Text.intercalate "/" [paymentsPath, paymentId, Refunds.refundsPath]

{-|
  Handler to get a refund by its identifier for a specific payment.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/get.
-}
getPaymentRefund :: Text.Text -- ^ paymentId
                 -> Text.Text -- ^ refundId
                 -> Mollie (Either ResponseError Refund)
getPaymentRefund paymentId refundId = get path
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, Refunds.refundsPath, refundId]

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
        path = Text.intercalate "/" [paymentsPath, paymentId, Refunds.refundsPath, refundId]

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
        path = Text.intercalate "/" [paymentsPath, paymentId, Refunds.refundsPath] <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
