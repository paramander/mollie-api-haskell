{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
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
    ) where

import           Control.Lens        ((&), (.~))
import           Data.Default        (def)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

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
    --
    -- Example for fetching the last customer:
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Customers
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let customersResult = runMollie env (getCustomersPaginated customerClient (Just 1) Nothing)
    -- @
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
                                      :> QueryParam "from" PaymentId
                                      :> Get '[HalJSON] (List Payment)
    -- ^Handler to get a paginated list of payments for a specific customer. Offset the results by passing the last payment ID in the `from` query param. The payment with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/customers-api/list-customers-payments
    --
    -- Example for fetching the last payment for a  customer:
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Customers
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let customerPaymentsResult = runMollie env (getCustomerPaymentsPaginated customerClient "cst_exampleid" (Just 1) Nothing)
    -- @
    , getCustomerPayments          :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> "payments"
                                      :> Get '[HalJSON] (List Payment)
    -- ^Handler to get a paginated list of payments for a specific customer. Applies default pagination for newest 250 payments. See https://docs.mollie.com/reference/v2/customers-api/list-customer-payments
    , createCustomerPayment        :: route :- "customers"
                                      :> Capture "id" CustomerId
                                      :> "payments"
                                      :> ReqBody '[JSON] NewPayment
                                      :> Post '[HalJSON] Payment
    -- ^Handler to create a new payment for a specific customer. See https://docs.mollie.com/reference/v2/customers-api/create-customer-payment
    } deriving Generic
