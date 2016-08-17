{-# LANGUAGE OverloadedStrings #-}

module Mollie.API.Subscriptions
    ( subscriptionsPath
    , newSubscription
    , createCustomerSubscription
    , getCustomerSubscription
    , getCustomerSubscriptions
    , cancelCustomerSubscription
    -- Re-export relevant types
    , PaymentMethod (..)
    , NewSubscription (..)
    , SubscriptionStatus (..)
    , SubscriptionLinks (..)
    , Subscription (..)
    , ListLinks (..)
    , List (..)
    , Failure (..)
    ) where

import qualified Data.Aeson           as Aeson
import           Data.Monoid
import qualified Data.Text            as Text
import           Mollie.API.Customers
import           Mollie.API.Internal
import           Mollie.API.Types
import qualified Network.HTTP.Types   as HTTP

{-|
  Subscriptions resource's path, relative to API's versioned customer resource url.
-}
subscriptionsPath :: Text.Text
subscriptionsPath = "subscriptions"

{-|
  Helper to create a minimal new subscription. Defaults to an ongoing subscription.
-}
newSubscription :: Double -> Text.Text -> Text.Text -> NewSubscription
newSubscription amount interval description = NewSubscription
    { newSubscription_amount      = amount
    , newSubscription_times       = Nothing
    , newSubscription_interval    = interval
    , newSubscription_description = description
    , newSubscription_method      = Nothing
    , newSubscription_webhookUrl  = Nothing
    }

{-|
  Handler to create a new subscription for a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/create.
-}
createCustomerSubscription :: Text.Text -> NewSubscription -> Mollie (Either Failure Subscription)
createCustomerSubscription customerId newSubscription = do
    (statusCode, rawBody) <- send HTTP.methodPost path newSubscription
    return $ case statusCode of
        201 -> case Aeson.decode rawBody of
            Just subscription -> Right subscription
            Nothing      -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [customersPath, customerId, subscriptionsPath]

{-|
  Handler to get a subscription by its identifier from a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/get.
-}
getCustomerSubscription :: Text.Text -> Text.Text -> Mollie (Either Failure Subscription)
getCustomerSubscription customerId subscriptionId = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just subscription -> Right subscription
            Nothing      -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [customersPath, customerId, subscriptionsPath, subscriptionId]

{-|
  Handler to get a list of subscriptions for a specific customer. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of subscriptions returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/list.
-}
getCustomerSubscriptions :: Text.Text -> Int -> Int -> Mollie (Either Failure (List Subscription))
getCustomerSubscriptions customerId offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just subscriptionList -> Right subscriptionList
            Nothing         -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = (Text.intercalate "/" [customersPath, customerId, subscriptionsPath]) <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count

{-|
  Handler to cancel a subscription by its identifier for a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/delete.
-}
cancelCustomerSubscription :: Text.Text -> Text.Text -> Mollie (Either Failure Subscription)
cancelCustomerSubscription customerId subscriptionId = do
    (statusCode, rawBody) <- delete path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just subscription -> Right subscription
            Nothing           -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [customersPath, customerId, subscriptionsPath, subscriptionId]
