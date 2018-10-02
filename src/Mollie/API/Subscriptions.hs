{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Subscriptions
    ( subscriptionsPath
    , newSubscription
    , createCustomerSubscription
    , getCustomerSubscription
    , getCustomerSubscriptions
    , cancelCustomerSubscription
    -- Re-export relevant types
    , NewSubscription (..)
    , SubscriptionStatus (..)
    , Subscription (..)
    -- Lens getters
    , Mollie.API.Subscriptions.id
    , mode
    , createdAt
    , status
    , amount
    , times
    , interval
    , startDate
    , description
    , method
    , canceledAt
    , webhookUrl
    ) where

import           Control.Lens         (makeFieldsNoPrefix, (&), (.~))
import           Data.Default         (def)
import           Data.Monoid
import qualified Data.Text            as Text
import qualified Mollie.API.Customers as Customers
import           Mollie.API.Internal
import           Mollie.API.Types
import qualified Network.HTTP.Types   as HTTP

makeFieldsNoPrefix ''NewSubscription
makeFieldsNoPrefix ''Subscription

{-|
  Subscriptions resource's path, relative to API's versioned customer resource url.
-}
subscriptionsPath :: Text.Text
subscriptionsPath = "subscriptions"

{-|
  Helper to create a minimal new subscription. Defaults to an ongoing subscription.

  The interval is in human readable format and support the following values:
  `1 day`/`n days`, `1 week`/`n weeks`, `1 month`/`n months` where n > 1.
-}
newSubscription :: Double -- ^ amount
                -> Text.Text -- ^ interval
                -> Text.Text -- ^ description
                -> NewSubscription
newSubscription _amount _interval _description =
    def
      & amount .~ (defaultAmount _amount)
      & interval .~ _interval
      & description .~ _description

{-|
  Handler to create a new subscription for a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/create.
-}
createCustomerSubscription :: Text.Text -- ^ customerId
                           -> NewSubscription -> Mollie (Either ResponseError Subscription)
createCustomerSubscription customerId newSubscription =
    decodeResult <$> send HTTP.methodPost path newSubscription
    where
        path = Text.intercalate "/" [Customers.customersPath, customerId, subscriptionsPath]

{-|
  Handler to get a subscription by its identifier from a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/get.
-}
getCustomerSubscription :: Text.Text -- ^ customerId
                        -> Text.Text -- ^ subscriptionId
                        -> Mollie (Either ResponseError Subscription)
getCustomerSubscription customerId subscriptionId = get path
    where
        path = Text.intercalate "/" [Customers.customersPath, customerId, subscriptionsPath, subscriptionId]

{-|
  Handler to get a list of subscriptions for a specific customer. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of subscriptions returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/list.
-}
getCustomerSubscriptions :: Text.Text -- ^ customerId
                         -> Int -- ^ offset
                         -> Int -- ^ count
                         -> Mollie (Either ResponseError (List Subscription))
getCustomerSubscriptions customerId offset count = get path
    where
        path = Text.intercalate "/" [Customers.customersPath, customerId, subscriptionsPath] <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count

{-|
  Handler to cancel a subscription by its identifier for a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/delete.
-}
cancelCustomerSubscription :: Text.Text -- ^ customerId
                           -> Text.Text -- ^ subscriptionId
                           -> Mollie (Either ResponseError Subscription)
cancelCustomerSubscription customerId subscriptionId =
    decodeResult <$> delete path
    where
        path = Text.intercalate "/" [Customers.customersPath, customerId, subscriptionsPath, subscriptionId]
