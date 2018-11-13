{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Customers
    ( CustomerAPI
    , newCustomer
    , createCustomer
    , getCustomer
    , getCustomers
    , getCustomersPaginated
    , createCustomerPayment
    , getCustomerPayments
    , getCustomerPaymentsPaginated
    , NewCustomer (..)
    , CustomerId
    , Customer (..)
    -- Lens getters
    , Mollie.API.Customers.id
    , mode
    , name
    , email
    , locale
    , metadata
    , recentlyUsedMethods
    , createdAt
    ) where

import           Control.Lens        (makeFieldsNoPrefix, (&), (.~))
import           Data.Aeson          ((.!=), (.:), (.:?))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Aeson.Types    as Aeson
import           Data.Default        (Default, def)
import qualified Data.Text           as Text
import qualified Data.Time           as Time
import           GHC.Generics        (Generic)
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Methods  (PaymentMethod (..))
import qualified Mollie.API.Payments as Payments
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

{-|
  Structure to request a new customer with.

  For more information see: https://www.mollie.com/en/docs/reference/customers/create.
-}
data NewCustomer = NewCustomer
    { _name     :: Maybe Text.Text
    -- ^Set the full name of the customer.
    , _email    :: Maybe Text.Text
    -- ^Set the email address.
    , _locale   :: Maybe Text.Text
    -- ^Set the language to use for this customer during checkout,
    , _metadata :: Maybe Aeson.Value
    -- ^Set any additional data in JSON format.
    }
    deriving (Show)

instance Default NewCustomer where
    def = NewCustomer
        { _name = def
        , _email = def
        , _locale = def
        , _metadata = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''NewCustomer)

makeFieldsNoPrefix ''NewCustomer

{-|
  Representation of an customer available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/customers/get.
-}
data Customer = Customer
    { _id                  :: CustomerId
    -- ^Mollies reference to the customer.
    , _mode                :: Mode
    -- ^The mode in which this customer was created.
    , _name                :: Maybe Text.Text
    -- ^The customers full name.
    , _email               :: Maybe Text.Text
    -- ^The cusomters email address.
    , _locale              :: Maybe Text.Text
    -- ^The locale used for this customer during checkout.
    , _metadata            :: Maybe Aeson.Value
    -- ^Custom privided data for this customer.
    , _recentlyUsedMethods :: [PaymentMethod]
    -- ^The payment methods this customer recently used.
    , _createdAt           :: Time.UTCTime
    -- ^The creation date of this customer.
    }
    deriving (Show)

instance Aeson.FromJSON Customer where
    parseJSON (Aeson.Object o) = do
        _id <- o .: "id"
        _mode <- o .: "mode"
        _name <- o .:? "name"
        _email <- o .:? "email"
        _locale <- o .:? "locale"
        _metadata <- o .:? "metadata"
        _recentlyUsedMethods <- o .:? "recentlyUsedMethods" .!= []
        _createdAt <- o .: "createdAt"

        return Customer{..}
    parseJSON invalid = Aeson.typeMismatch "Customer" invalid

makeFieldsNoPrefix ''Customer

{-|
  Helper to create a minimal new customer.
-}
newCustomer :: Text.Text -- ^ _name
            -> Text.Text -- ^ _email
            -> NewCustomer
newCustomer _name _email =
    def
      & name .~ Just _name
      & email .~ Just _email

data CustomerAPI route = CustomerAPI
    { getCustomersPaginated        :: route :- "customers"
                                      :> QueryParam "limit" Int
                                      :> QueryParam "from" CustomerId
                                      :> Get '[HalJSON] (List Customer)
    -- ^Handler to get a paginated list of customers. Offset the results by passing the last customer ID in the `from` query param. The customer with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/customers-api/list-customers
    , getCustomers                 :: route :- "customers"
                                      :> Get '[HalJSON] (List Customer)
    -- ^Handler to get a paginated list of customers. Applies default pagination for newest 250 customers. See https://docs.mollie.com/reference/v2/customers-api/list-customers
    , createCustomer               :: route :- "customers"
                                      :> ReqBody '[JSON] NewCustomer
                                      :> Post '[HalJSON] Customer
    -- ^Handler to create a new customer. See https://docs.mollie.com/reference/v2/customers-api/create-customer
    , getCustomer                  :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> Get '[HalJSON] Customer
    -- ^Handler to get a customer by its identifier. See https://docs.mollie.com/reference/v2/customers-api/get-customer
    , getCustomerPaymentsPaginated :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> "payments"
                                      :> QueryParam "limit" Int
                                      :> QueryParam "from" Payments.PaymentId
                                      :> Get '[HalJSON] (List Payments.Payment)
    -- ^Handler to get a paginated list of payments for a specific customer. Offset the results by passing the last payment ID in the `from` query param. The payment with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/customers-api/list-customers-payments
    , getCustomerPayments          :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> "payments"
                                      :> Get '[HalJSON] (List Payments.Payment)
    -- ^Handler to get a paginated list of payments for a specific customer. Applies default pagination for newest 250 payments. See https://docs.mollie.com/reference/v2/customers-api/list-customer-payments
    , createCustomerPayment        :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> "payments"
                                      :> ReqBody '[JSON] Payments.NewPayment
                                      :> Post '[HalJSON] Payments.Payment
    -- ^Handler to create a new payment for a specific customer. See https://docs.mollie.com/reference/v2/customers-api/create-customer-payment
    } deriving Generic
