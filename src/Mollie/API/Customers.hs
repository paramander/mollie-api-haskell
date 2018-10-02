{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Customers
    ( customersPath
    , newCustomer
    , createCustomer
    , getCustomer
    , getCustomers
    , createCustomerPayment
    , getCustomerPayments
    -- Re-export relevant types
    , NewCustomer (..)
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
import           Data.Default        (def)
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import qualified Mollie.API.Payments as Payments
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP

makeFieldsNoPrefix ''NewCustomer
makeFieldsNoPrefix ''Customer

{-|
  Customer resource's path, relative to API's versioned url.
-}
customersPath :: Text.Text
customersPath = "customers"

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

{-|
  Handler to create a new customer.

  For more information see: https://www.mollie.com/en/docs/reference/customers/create.
-}
createCustomer :: NewCustomer -> Mollie (Either ResponseError Customer)
createCustomer newCustomer =
    decodeResult <$> send HTTP.methodPost path newCustomer
    where
        path = customersPath

{-|
  Handler to get a customer by its identifier.

  For more information see: https://www.mollie.com/en/docs/reference/customers/get.
-}
getCustomer :: Text.Text -- ^ customerId
            -> Mollie (Either ResponseError Customer)
getCustomer customerId = get path
    where
        path = Text.intercalate "/" [customersPath, customerId]

{-|
  Handler to get a list of customers. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of customers returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/customers/list.
-}
getCustomers :: Int -- ^ offset
             -> Int -- ^ count
             -> Mollie (Either ResponseError (List Customer))
getCustomers offset count = get path
    where
        path = customersPath <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count

{-|
  Handler to create a new payment for a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/customers/create-payment.
-}
createCustomerPayment :: Text.Text -- ^ customerId
                      -> NewPayment -> Mollie (Either ResponseError Payment)
createCustomerPayment customerId newPayment =
    decodeResult <$> send HTTP.methodPost path newPayment
    where
        path = Text.intercalate "/" [customersPath, customerId, Payments.paymentsPath]

{-|
  Handler to get a list of payments for a specific customer. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payments returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/customers/list-payments.
-}
getCustomerPayments :: Text.Text -- ^ customerId
                    -> Int -- ^ offset
                    -> Int -- ^ count
                    -> Mollie (Either ResponseError (List Payment))
getCustomerPayments customerId offset count = get path
    where
        path = Text.intercalate "/" [customersPath, customerId, Payments.paymentsPath] <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
