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
newCustomer :: Text.Text -- ^ name
            -> Text.Text -- ^ email
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
    , getCustomers                 :: route :- "customers"
                                      :> Get '[HalJSON] (List Customer)
    , createCustomer               :: route :- "customers"
                                      :> ReqBody '[JSON] NewCustomer
                                      :> Post '[HalJSON] Customer
    , getCustomer                  :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> Get '[HalJSON] Customer
    , getCustomerPaymentsPaginated :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> "payments"
                                      :> QueryParam "limit" Int
                                      :> QueryParam "from" Payments.PaymentId
                                      :> Get '[HalJSON] (List Payments.Payment)
    , getCustomerPayments          :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> "payments"
                                      :> Get '[HalJSON] (List Payments.Payment)
    , createCustomerPayment        :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> "payments"
                                      :> ReqBody '[JSON] Payments.NewPayment
                                      :> Post '[HalJSON] Payments.Payment
    } deriving Generic
