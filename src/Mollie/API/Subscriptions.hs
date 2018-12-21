{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Subscriptions
    ( SubscriptionAPI
    , newSubscription
    , createCustomerSubscription
    , getCustomerSubscription
    , getCustomerSubscriptions
    , getCustomerSubscriptionsPaginated
    , cancelCustomerSubscription
    , getSubscriptionPayments
    , getSubscriptionPaymentsPaginated
    ) where

import           Control.Lens        ((&), (.~))
import           Data.Default        (Default, def)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

{-|
  Servant API definition for Mollie subscriptions API https://www.mollie.com/en/docs/reference/subscriptions/get.
-}
data SubscriptionAPI route = SubscriptionAPI
    { getCustomerSubscriptionsPaginated :: route :- "customers"
                                           :> Capture "customerId" CustomerId
                                           :> "subscriptions"
                                           :> QueryParam "limit" Int
                                           :> QueryParam "from" SubscriptionId
                                           :> Get '[HalJSON] (List Subscription)
    -- ^Handler to get a paginated list of customers. Offset the results by passing the last customer ID in the `from` query param. The customer with this ID is included in the result set as well. See https://www.mollie.com/en/docs/reference/subscriptions/list
    --
    -- Example for fetching the last subscription of customer with id "cst_exampleid":
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Subscriptions
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let customerSubscriptionsResult = runMollie env (getCustomerSubscriptionsPaginated subscriptionClient "cst_exampleid" (Just 1) Nothing)
    -- @
    , getCustomerSubscriptions          :: route :- "customers"
                                           :> Capture "customerId" CustomerId
                                           :> "subscriptions"
                                           :> Get '[HalJSON] (List Subscription)
    -- ^Handler to get a paginated list of subscriptions for a specific customer. Applies default pagination for newest 250 subscriptions. See https://www.mollie.com/en/docs/reference/subscriptions/list
    , createCustomerSubscription        :: route :- "customers"
                                           :> Capture "customerId" CustomerId
                                           :> "subscriptions"
                                           :> ReqBody '[JSON] NewSubscription
                                           :> Post '[HalJSON] Subscription
    -- ^Handler to create a new subscription for a specific customer. See https://www.mollie.com/en/docs/reference/subscriptions/create
    , getCustomerSubscription           :: route :- "customers"
                                           :> Capture "customerId" CustomerId
                                           :> "subscriptions"
                                           :> Capture "id" SubscriptionId
                                           :> Get '[HalJSON] Subscription
    -- ^Handler to get a subscription by its identifier from a specific customer. See https://www.mollie.com/en/docs/reference/subscriptions/get
    , cancelCustomerSubscription        :: route :- "customers"
                                           :> Capture "customerId" CustomerId
                                           :> "subscriptions"
                                           :> Capture "id" SubscriptionId
                                           :> DeleteNoContent '[HalJSON] NoContent
    -- ^Handler to cancel a subscription by its identifier for a specific customer. See https://www.mollie.com/en/docs/reference/subscriptions/delete
    , getSubscriptionPaymentsPaginated  :: route :- "customers"
                                           :> Capture "customerId" CustomerId
                                           :> "subscriptions"
                                           :> Capture "id" SubscriptionId
                                           :> "payments"
                                           :> QueryParam "limit" Int
                                           :> QueryParam "from" PaymentId
                                           :> Get '[HalJSON] (List Payment)
    -- ^Handler to get a paginated list of payments of a specific subscription. Offset the result set to the payment with `from` query param. The payment with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/subscriptions-api/list-subscriptions-payments
    --
    -- Example for fetching the last 5 payments of a subscription with 'CustomerId' "cst_exampleid" and 'SubscriptionId' "sub_exampleid":
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Subscriptions
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let subscriptionPaymentsResult = runMollie env (getSubscriptionPaymentsPaginated subscriptionClient "cst_exampleid" "sub_exampleid" (Just 5) Nothing)
    -- @
    , getSubscriptionPayments           :: route :- "customers"
                                           :> Capture "customerId" CustomerId
                                           :> "subscriptions"
                                           :> Capture "id" SubscriptionId
                                           :> "payments"
                                           :> Get '[HalJSON] (List Payment)
    -- ^Handler to get a paginated list of payments of a specific subscription. Applies default pagination for newest 250 payments. See https://docs.mollie.com/reference/v2/subscriptions-api/list-subscriptions-payments
    } deriving Generic

{-|
  Helper to create a minimal new subscription. Defaults to an ongoing subscription.

  The interval is in human readable format and support the following values:
  `1 day`/`n days`, `1 week`/`n weeks`, `1 month`/`n months` where n > 1.
-}
newSubscription :: Double -- ^ _amount
                -> Text.Text -- ^ _interval
                -> Text.Text -- ^ _description
                -> NewSubscription
newSubscription _amount _interval _description =
    def
      & amount .~ (defaultAmount _amount)
      & interval .~ _interval
      & description .~ _description
