{-# LANGUAGE OverloadedStrings #-}

{-|
  [WARNING]: This implementation is currently untested! Due to lack of access to the Mandates API.
-}
module Mollie.API.Mandates
    ( mandatesPath
    , newMandate
    , createCustomerMandate
    , getCustomerMandate
    , getCustomerMandates
    -- Re-export relevant types
    , PaymentMethod (..)
    , NewMandate (..)
    , MandateStatus (..)
    , MandateDetails (..)
    , Mandate (..)
    , ListLinks (..)
    , List (..)
    , ResponseError (..)
    ) where

import           Data.Monoid
import qualified Data.Text            as Text
import           Mollie.API.Customers
import           Mollie.API.Internal
import           Mollie.API.Types
import qualified Network.HTTP.Types   as HTTP

{-|
  Mandates resource's path, relative to API's versioned customer resource url.
-}
mandatesPath :: Text.Text
mandatesPath = "mandates"

newMandate :: PaymentMethod
           -> Text.Text -- ^ consumerName
           -> Text.Text -- ^ consumerAccount
           -> NewMandate
newMandate method consumerName consumerAccount = NewMandate
    { newMandate_method           = method
    , newMandate_consumerName     = consumerName
    , newMandate_consumerAccount  = consumerAccount
    , newMandate_consumerBic      = Nothing
    , newMandate_signatureDate    = Nothing
    , newMandate_mandateReference = Nothing
    }

{-|
  Handler to create a new mandate for a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/create.
-}
createCustomerMandate :: Text.Text -- ^ customerId
                      -> NewMandate -> Mollie (Either ResponseError Mandate)
createCustomerMandate customerId newMandate =
    decodeResult <$> send HTTP.methodPost path newMandate
    where
        path = Text.intercalate "/" [customersPath, customerId, mandatesPath]

{-|
  Handler to get a mandate by its identifier from a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/get.
-}
getCustomerMandate :: Text.Text -- ^ customerId
                   -> Text.Text -- ^ mandateId
                   -> Mollie (Either ResponseError Mandate)
getCustomerMandate customerId mandateId = get path
    where
        path = Text.intercalate "/" [customersPath, customerId, mandatesPath, mandateId]

{-|
  Handler to get a list of mandates for a specific customer. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of mandates returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/list.
-}
getCustomerMandates :: Text.Text -- ^ customerId
                    -> Int -- ^ offset
                    -> Int -- ^ count
                    -> Mollie (Either ResponseError (List Mandate))
getCustomerMandates customerId offset count = get path
    where
        path = Text.intercalate "/" [customersPath, customerId, mandatesPath] <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
