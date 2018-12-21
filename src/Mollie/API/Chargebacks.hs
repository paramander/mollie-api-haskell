{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Chargebacks
    ( ChargebackAPI
    , getChargebacks
    , getChargebacksPaginated
    , getChargeback
    ) where

import           GHC.Generics        (Generic)
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

data ChargebackAPI route = ChargebackAPI
    { getChargebacksPaginated        :: route :- "chargebacks"
                                        :> QueryParam "limit" Int
                                        :> QueryParam "from" ChargebackId
                                        :> Get '[HalJSON] (List Chargeback)
    -- ^Handler to get a paginated list of chargebacks. Offset the results by passing the last chargeback ID in the `from` query param. The chargeback with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/chargebacks-api/list-chargebacks
    --
    -- Example for fetching the last chargeback:
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Chargebacks
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let chargebacksResult = runMollie env (getChargebacksPaginated chargebackClient (Just 1) Nothing)
    -- @
    , getChargebacks                 :: route :- "chargebacks"
                                        :> Get '[HalJSON] (List Chargeback)
    -- ^Handler to get a paginated list of chargebacks. Applies default pagination for newest 250 chargebacks. See https://docs.mollie.com/reference/v2/chargebacks-api/list-chargebacks
    , getChargeback                  :: route :- "payments"
                                        :> Capture "paymentId" PaymentId
                                        :> "chargebacks"
                                        :> Capture "id" ChargebackId
                                        :> Get '[HalJSON] Chargeback
    -- ^Handler to get a specific chargeback for a specific payment. See https://docs.mollie.com/reference/v2/chargebacks-api/get-chargeback
    } deriving Generic
