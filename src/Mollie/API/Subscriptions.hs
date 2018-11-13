{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
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
    , cancelCustomerSubscription
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
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import           Data.Default         (Default, def)
import           Data.Proxy           ()
import qualified Data.Text            as Text
import qualified Data.Time            as Time
import           GHC.Generics         (Generic)
import qualified Mollie.API.Customers as Customers
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
    { _amount      :: Amount
    -- ^Set the amount you want to charge each subscription cycle.
    , _times       :: Maybe Int
    -- ^Set the total number of charges for the subscription to complete. Leave empty for ongoing subscriptions.
    , _interval    :: Text.Text
    -- ^Set the interval to wait between charges like `1 month(s)`, `2 weeks` or `14 days`.
    , _startDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format. This is the first day on which your customer will be charged. When this parameter is not provided, the current date will be used instead.
    , _description :: Text.Text
    -- ^Set the description which will be included in the payment description along with the carge date in `Y-m-d` format.
    , _method      :: Maybe PaymentMethod
    -- ^Force the payment method, leave empty to use one of the customers valid mandates.
    , _webhookUrl  :: Maybe Text.Text
    -- ^Set a webhook URL for all subscription payments.
    }
    deriving (Show)

instance Default NewSubscription where
    def = NewSubscription
        { _amount = def
        , _times = def
        , _interval = mempty
        , _startDate = def
        , _description = mempty
        , _method = def
        , _webhookUrl = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''NewSubscription)

makeFieldsNoPrefix ''NewSubscription

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
    { _id          :: Text.Text
    -- ^Mollies reference to the subscription.
    , _mode        :: Mode
    -- ^The mode used to create this subscription
    , _createdAt   :: Time.UTCTime
    -- ^The date on which this subscription was created.
    , _status      :: SubscriptionStatus
    -- ^The subscriptions status.
    , _amount      :: Amount
    -- ^The amount charged with each payment for this subscription.
    , _times       :: Maybe Int
    -- ^The total number or charges for the subscription to complete.
    , _interval    :: Text.Text
    -- ^The interval to wait between charges.
    , _startDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format.
    , _description :: Text.Text
    -- ^The description for the payments made with this subscription.
    , _method      :: Maybe PaymentMethod
    -- ^The payment method used for this subscription.
    , _canceledAt  :: Maybe Time.UTCTime
    -- ^The date on which this subscription was canceled.
    , _webhookUrl  :: Maybe Text.Text
    -- ^The URL Mollie will call as soon a payment status change takes place.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Subscription)

makeFieldsNoPrefix ''Subscription

data SubscriptionAPI route = SubscriptionAPI
    { getCustomerSubscriptions   :: route :- "customers"
                                    :> Capture "customerId" Customers.CustomerId
                                    :> "subscriptions"
                                    :> QueryParam "limit" Int
                                    :> QueryParam "from" SubscriptionId
                                    :> Get '[HalJSON] (List Subscription)
    , createCustomerSubscription :: route :- "customers"
                                    :> Capture "customerId" Customers.CustomerId
                                    :> "subscriptions"
                                    :> ReqBody '[JSON] NewSubscription
                                    :> Post '[HalJSON] Subscription
    , getCustomerSubscription    :: route :- "customers"
                                    :> Capture "customerId" Customers.CustomerId
                                    :> "subscriptions"
                                    :> Capture "id" SubscriptionId
                                    :> Get '[HalJSON] Subscription
    , cancelCustomerSubscription :: route :- "customers"
                                    :> Capture "customerId" Customers.CustomerId
                                    :> "subscriptions"
                                    :> Capture "id" SubscriptionId
                                    :> DeleteNoContent '[HalJSON] NoContent
    , getSubscriptionPayments    :: route :- "customers"
                                    :> Capture "customerId" Customers.CustomerId
                                    :> "subscriptions"
                                    :> Capture "id" SubscriptionId
                                    :> "payments"
                                    :> QueryParam "limit" Int
                                    :> QueryParam "from" Payments.PaymentId
                                    :> Get '[HalJSON] (List Payments.Payment)
    } deriving Generic

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
