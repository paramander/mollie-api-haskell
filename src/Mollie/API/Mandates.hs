{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Mandates
    ( MandateAPI
    , newMandate
    , createCustomerMandate
    , getCustomerMandate
    , getCustomerMandates
    , getCustomerMandatesPaginated
    , revokeCustomerMandate
    ) where

import           Control.Lens         ((&), (.~))
import           Data.Default         (def)
import qualified Data.Text            as Text
import           GHC.Generics         (Generic)
import           Mollie.API.Helpers
import           Mollie.API.Internal  (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

newMandate :: PaymentMethod -- ^ _method
           -> Text.Text -- ^ _consumerName
           -> Text.Text -- ^ _consumerAccount
           -> NewMandate
newMandate _method _consumerName _consumerAccount =
    def
      & method .~ _method
      & consumerName .~ _consumerName
      & consumerAccount .~ _consumerAccount

data MandateAPI route = MandateAPI
    { getCustomerMandatesPaginated :: route :- "customers"
                                      :> Capture "customerId" CustomerId
                                      :> "mandates"
                                      :> QueryParam "limit" Int
                                      :> QueryParam "from" MandateId
                                      :> Get '[HalJSON] (List Mandate)
    -- ^Handler to get a paginated list of mandates for a specific customer. Offset the results by passing the last mandate ID in the `from` query param. The mandate with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/mandates-api/list-mandates
    --
    -- Example for fetching the last mandate for a customer:
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Mandates
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let customerMandatesResult = runMollie env (getCustomerMandatesPaginated customerClient "cst_exampleid" (Just 1) Nothing)
    -- @
    , getCustomerMandates          :: route :- "customers"
                                      :> Capture "customerId" CustomerId
                                      :> "mandates"
                                      :> Get '[HalJSON] (List Mandate)
    -- ^Handler to get a paginated list of mandates for a specific customer. Applies default pagination for newest 250 customers. See https://docs.mollie.com/reference/v2/mandates-api/list-mandates
    , createCustomerMandate        :: route :- "customers"
                                      :> Capture "customerId" CustomerId
                                      :> "mandates"
                                      :> ReqBody '[JSON] NewMandate
                                      :> Post '[HalJSON] Mandate
    -- ^Handler to create a new mandate for a specific customer. See https://docs.mollie.com/reference/v2/mandates-api/create-mandate
    , getCustomerMandate           :: route :- "customers"
                                      :> Capture "customerId" CustomerId
                                      :> "mandates"
                                      :> Capture "id" MandateId
                                      :> Get '[HalJSON] Mandate
    -- ^Handler to get a mandate by its identifier for a specific customer. See https://docs.mollie.com/reference/v2/mandates-api/get-mandate
    , revokeCustomerMandate           :: route :- "customers"
                                      :> Capture "customerId" CustomerId
                                      :> "mandates"
                                      :> Capture "mandateId" MandateId
                                      :> DeleteNoContent '[HalJSON] NoContent
    -- ^Handler to remove a mandate by its identifier for a specific customer. See https://docs.mollie.com/reference/v2/mandates-api/revoke-mandate
    } deriving Generic
