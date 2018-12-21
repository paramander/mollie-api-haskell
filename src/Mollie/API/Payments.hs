{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Payments
    ( PaymentAPI
    , newPayment
    , newRecurringPayment
    , createPayment
    , getPayment
    , getPayments
    , getPaymentsPaginated
    , cancelPayment
    ) where

import           Control.Lens        ((&), (.~))
import           Data.Default        (def)
import qualified Data.Text           as Text
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

{-|
  Helper to create a minimal new payment for normal use.
-}
newPayment :: Double -- ^ _amount
           -> Text.Text -- ^ _description
           -> Text.Text -- ^ _redirectUrl
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
newRecurringPayment :: Double -- ^ _amount
                    -> Text.Text -- ^ _description
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

data PaymentAPI route = PaymentAPI
    { getPaymentsPaginated :: route :- "payments"
                              :> QueryParam "limit" Int
                              :> QueryParam "from" PaymentId
                              :> Get '[HalJSON] (List Payment)
    -- ^Handler to get a paginated list of payments. Offset the results by passing the last payment ID in the `from` query param. The payment with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/payments-api/list-payments
    --
    -- Example for fetching the last 10 payments:
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Payments
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let paymentsResult = runMollie env (getPaymentsPaginated paymentClient (Just 10) Nothing)
    -- @
    , getPayments          :: route :- "payments"
                              :> Get '[HalJSON] (List Payment)
    -- ^Handler to get a paginated list of payments. Applies default pagination for newest 250 customers. See https://docs.mollie.com/reference/v2/payments-api/list-payments
    , createPayment        :: route :- "payments"
                              :> ReqBody '[JSON] NewPayment
                              :> Post '[HalJSON] Payment
    -- ^Handler to create a new payment. See https://docs.mollie.com/reference/v2/payments-api/create-payment
    , getPayment           :: route :- "payments"
                              :> Capture "id" PaymentId
                              :> Get '[HalJSON] Payment
    -- ^Handler to get a payment by its identifier. See https://docs.mollie.com/reference/v2/payments-api/create-payment
    , cancelPayment        :: route :- "payments"
                              :> Capture "id" PaymentId
                              :> Delete '[HalJSON] Payment
    -- ^Handler to cancel a payment by its identifier. See https://docs.mollie.com/reference/v2/payments-api/cancel-payment
    } deriving Generic
