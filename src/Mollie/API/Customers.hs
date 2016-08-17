{-# LANGUAGE OverloadedStrings #-}

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
    , Mode (..)
    , PaymentStatus (..)
    , PaymentMethod (..)
    , RecurringType (..)
    , NewPayment (..)
    , Payment (..)
    , PaymentLinks (..)
    , List (..)
    , ListLinks (..)
    , Failure (..)
    ) where

import qualified Data.Aeson          as Aeson
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Payments
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP

{-|
  Customer resource's path, relative to API's versioned url.
-}
customersPath :: Text.Text
customersPath = "customers"

{-|
  Helper to create a minimal new customer.
-}
newCustomer :: Text.Text -> Text.Text -> NewCustomer
newCustomer name email = NewCustomer
    { newCustomer_name     = name
    , newCustomer_email    = email
    , newCustomer_locale   = Nothing
    , newCustomer_metadata = Nothing
    }

{-|
  Handler to create a new customer.

  For more information see: https://www.mollie.com/en/docs/reference/customers/create.
-}
createCustomer :: NewCustomer -> Mollie (Either Failure Customer)
createCustomer newCustomer = do
    (statusCode, rawBody) <- send HTTP.methodPost path newCustomer
    return $ case statusCode of
        201 -> case Aeson.decode rawBody of
            Just customer -> Right customer
            Nothing       -> Left $ ParseFailure rawBody
        _ -> Left $ RequestFailure statusCode rawBody
    where
        path = customersPath

{-|
  Handler to get a customer by its identifier.

  For more information see: https://www.mollie.com/en/docs/reference/customers/get.
-}
getCustomer :: Text.Text -> Mollie (Either Failure Customer)
getCustomer customerId = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just customer -> Right customer
            Nothing      -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [customersPath, customerId]

{-|
  Handler to get a list of customers. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of customers returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/customers/list.
-}
getCustomers :: Int -> Int -> Mollie (Either Failure (List Customer))
getCustomers offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just customerList -> Right customerList
            Nothing         -> Left $ ParseFailure rawBody
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = customersPath <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count

{-|
  Handler to create a new payment for a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/customers/create-payment.
-}
createCustomerPayment :: Text.Text -> NewPayment -> Mollie (Either Failure Payment)
createCustomerPayment customerId newPayment = do
    (statusCode, rawBody) <- send HTTP.methodPost path newPayment
    return $ case statusCode of
        201 -> case Aeson.decode rawBody of
            Just payment -> Right payment
            Nothing     -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [customersPath, customerId, paymentsPath]

{-|
  Handler to get a list of payments for a specific customer. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payments returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/customers/list-payments.
-}
getCustomerPayments :: Text.Text -> Int -> Int -> Mollie (Either Failure (List Payment))
getCustomerPayments customerId offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just paymentList -> Right paymentList
            Nothing         -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = (Text.intercalate "/" [customersPath, customerId, paymentsPath]) <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
