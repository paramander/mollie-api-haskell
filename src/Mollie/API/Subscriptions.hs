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

import           Control.Lens         (makeFields, (&), (.~))
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import           Data.Default         (Default, def)
import           Data.Proxy           ()
import qualified Data.Text            as Text
import qualified Data.Time            as Time
import           GHC.Generics         (Generic)
import qualified Mollie.API.Customers as Customers
import           Mollie.API.Helpers
import           Mollie.API.Internal  (HalJSON)
import           Mollie.API.Methods   (PaymentMethod (..))
import qualified Mollie.API.Payments  as Payments
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

{-|
  Structure to request a new subscription with.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/create.
-}
data NewSubscription = NewSubscription
    { _newSubscriptionAmount      :: Amount
    -- ^Set the amount you want to charge each subscription cycle.
    , _newSubscriptionTimes       :: Maybe Int
    -- ^Set the total number of charges for the subscription to complete. Leave empty for ongoing subscriptions.
    , _newSubscriptionInterval    :: Text.Text
    -- ^Set the interval to wait between charges like `1 month(s)`, `2 weeks` or `14 days`.
    , _newSubscriptionStartDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format. This is the first day on which your customer will be charged. When this parameter is not provided, the current date will be used instead.
    , _newSubscriptionDescription :: Text.Text
    -- ^Set the description which will be included in the payment description along with the carge date in `Y-m-d` format.
    , _newSubscriptionMethod      :: Maybe PaymentMethod
    -- ^Force the payment method, leave empty to use one of the customers valid mandates.
    , _newSubscriptionWebhookUrl  :: Maybe Text.Text
    -- ^Set a webhook URL for all subscription payments.
    }
    deriving (Show)

instance Default NewSubscription where
    def = NewSubscription
        { _newSubscriptionAmount = def
        , _newSubscriptionTimes = def
        , _newSubscriptionInterval = mempty
        , _newSubscriptionStartDate = def
        , _newSubscriptionDescription = mempty
        , _newSubscriptionMethod = def
        , _newSubscriptionWebhookUrl = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 16
        }
    ''NewSubscription)

makeFields ''NewSubscription

{-|
  All possible statusses a subscription could be assigned.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/get.
-}
data SubscriptionStatus
    = SubscriptionPending
    | SubscriptionActive
    | SubscriptionCancelled
    | SubscriptionSuspended
    | SubscriptionCompleted
    deriving (Read, Show, Eq)

instance ToText SubscriptionStatus where
    toText = Text.pack . Aeson.camelTo2 '_' . drop 12 . show

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_' . drop 12
        }
    ''SubscriptionStatus)

{-|
  Representation of a subscription available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/get.
-}
data Subscription = Subscription
    { _subscriptionId          :: Text.Text
    -- ^Mollies reference to the subscription.
    , _subscriptionMode        :: Mode
    -- ^The mode used to create this subscription
    , _subscriptionCreatedAt   :: Time.UTCTime
    -- ^The date on which this subscription was created.
    , _subscriptionStatus      :: SubscriptionStatus
    -- ^The subscriptions status.
    , _subscriptionAmount      :: Amount
    -- ^The amount charged with each payment for this subscription.
    , _subscriptionTimes       :: Maybe Int
    -- ^The total number or charges for the subscription to complete.
    , _subscriptionInterval    :: Text.Text
    -- ^The interval to wait between charges.
    , _subscriptionStartDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format.
    , _subscriptionDescription :: Text.Text
    -- ^The description for the payments made with this subscription.
    , _subscriptionMethod      :: Maybe PaymentMethod
    -- ^The payment method used for this subscription.
    , _subscriptionCanceledAt  :: Maybe Time.UTCTime
    -- ^The date on which this subscription was canceled.
    , _subscriptionWebhookUrl  :: Maybe Text.Text
    -- ^The URL Mollie will call as soon a payment status change takes place.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 13
        }
    ''Subscription)

makeFields ''Subscription

{-|
  Servant API definition for Mollie subscriptions API https://www.mollie.com/en/docs/reference/subscriptions/get.
-}
data SubscriptionAPI route = SubscriptionAPI
    { getCustomerSubscriptionsPaginated :: route :- "customers"
                                           :> Capture "customerId" Customers.CustomerId
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
                                           :> Capture "customerId" Customers.CustomerId
                                           :> "subscriptions"
                                           :> Get '[HalJSON] (List Subscription)
    -- ^Handler to get a paginated list of subscriptions for a specific customer. Applies default pagination for newest 250 subscriptions. See https://www.mollie.com/en/docs/reference/subscriptions/list
    , createCustomerSubscription        :: route :- "customers"
                                           :> Capture "customerId" Customers.CustomerId
                                           :> "subscriptions"
                                           :> ReqBody '[JSON] NewSubscription
                                           :> Post '[HalJSON] Subscription
    -- ^Handler to create a new subscription for a specific customer. See https://www.mollie.com/en/docs/reference/subscriptions/create
    , getCustomerSubscription           :: route :- "customers"
                                           :> Capture "customerId" Customers.CustomerId
                                           :> "subscriptions"
                                           :> Capture "id" SubscriptionId
                                           :> Get '[HalJSON] Subscription
    -- ^Handler to get a subscription by its identifier from a specific customer. See https://www.mollie.com/en/docs/reference/subscriptions/get
    , cancelCustomerSubscription        :: route :- "customers"
                                           :> Capture "customerId" Customers.CustomerId
                                           :> "subscriptions"
                                           :> Capture "id" SubscriptionId
                                           :> DeleteNoContent '[HalJSON] NoContent
    -- ^Handler to cancel a subscription by its identifier for a specific customer. See https://www.mollie.com/en/docs/reference/subscriptions/delete
    , getSubscriptionPaymentsPaginated  :: route :- "customers"
                                           :> Capture "customerId" Customers.CustomerId
                                           :> "subscriptions"
                                           :> Capture "id" SubscriptionId
                                           :> "payments"
                                           :> QueryParam "limit" Int
                                           :> QueryParam "from" Payments.PaymentId
                                           :> Get '[HalJSON] (List Payments.Payment)
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
                                           :> Capture "customerId" Customers.CustomerId
                                           :> "subscriptions"
                                           :> Capture "id" SubscriptionId
                                           :> "payments"
                                           :> Get '[HalJSON] (List Payments.Payment)
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
