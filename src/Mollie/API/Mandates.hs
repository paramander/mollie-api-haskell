{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

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
    , NewMandate (..)
    , MandateStatus (..)
    , MandateDetails (..)
    , Mandate (..)
    -- Lens getters
    , Mollie.API.Mandates.id
    , status
    , method
    , details
    , mandateReference
    , signatureDate
    , createdAt
    , consumerName
    , consumerAccount
    , consumerBic
    , cardHolder
    , cardNumber
    , cardLabel
    , cardFingerprint
    , cardExpiryDate
    ) where

import           Control.Lens         (makeFieldsNoPrefix, (&), (.~))
import           Data.Default         (def)
import           Data.Monoid
import qualified Data.Text            as Text
import qualified Mollie.API.Customers as Customers
import           Mollie.API.Internal
import           Mollie.API.Types
import qualified Network.HTTP.Types   as HTTP

makeFieldsNoPrefix ''NewMandate
makeFieldsNoPrefix ''MandateDetails
makeFieldsNoPrefix ''Mandate

{-|
  Mandates resource's path, relative to API's versioned customer resource url.
-}
mandatesPath :: Text.Text
mandatesPath = "mandates"

newMandate :: PaymentMethod
           -> Text.Text -- ^ consumerName
           -> Text.Text -- ^ consumerAccount
           -> NewMandate
newMandate _method _consumerName _consumerAccount =
    def
      & method .~ _method
      & consumerName .~ _consumerName
      & consumerAccount .~ _consumerAccount

{-|
  Handler to create a new mandate for a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/create.
-}
createCustomerMandate :: Text.Text -- ^ customerId
                      -> NewMandate -> Mollie (Either ResponseError Mandate)
createCustomerMandate customerId newMandate =
    decodeResult <$> send HTTP.methodPost path newMandate
    where
        path = Text.intercalate "/" [Customers.customersPath, customerId, mandatesPath]

{-|
  Handler to get a mandate by its identifier from a specific customer.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/get.
-}
getCustomerMandate :: Text.Text -- ^ customerId
                   -> Text.Text -- ^ mandateId
                   -> Mollie (Either ResponseError Mandate)
getCustomerMandate customerId mandateId = get path
    where
        path = Text.intercalate "/" [Customers.customersPath, customerId, mandatesPath, mandateId]

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
        path = Text.intercalate "/" [Customers.customersPath, customerId, mandatesPath] <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
