{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Refunds
    ( RefundAPI
    , getRefunds
    , getRefundsPaginated
    , newRefund
    , createPaymentRefund
    , getPaymentRefund
    , cancelPaymentRefund
    ) where

import           Data.Default        (Default, def)
import           GHC.Generics        (Generic)
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

data RefundAPI route = RefundAPI
    { getRefundsPaginated        :: route :- "refunds"
                                    :> QueryParam "limit" Int
                                    :> QueryParam "from" RefundId
                                    :> Get '[HalJSON] (List Refund)
    -- ^Handler to get a paginated list of refunds. Offset the results by passing the last refund ID in the `from` query param. The refund with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/refunds-api/list-refunds
    --
    -- Example for fetching the last 3 refunds:
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Refunds
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let refundsResult = runMollie env (getRefundsPaginated refundClient (Just 3) Nothing)
    -- @
    , getRefunds                 :: route :- "refunds"
                                    :> Get '[HalJSON] (List Refund)
    -- ^Handler to get a paginated list of refunds. Applies default pagination for newest 250 customers. See https://docs.mollie.com/reference/v2/refunds-api/list-refunds
    , createPaymentRefund        :: route :- "payments"
                                    :> Capture "paymentId" PaymentId
                                    :> "refunds"
                                    :> ReqBody '[JSON] NewRefund
                                    :> Post '[HalJSON] Refund
    -- ^Handler to create a new refund for a specific payment. See https://docs.mollie.com/reference/v2/refunds-api/create-refund
    , getPaymentRefund           :: route :- "payments"
                                    :> Capture "paymentId" PaymentId
                                    :> "refunds"
                                    :> Capture "id" RefundId
                                    :> Get '[HalJSON] Refund
    -- ^Handler to get a refund by its identifier for a specific payment. See https://docs.mollie.com/reference/v2/refunds-api/get-refund
    , cancelPaymentRefund        :: route :- "payments"
                                    :> Capture "paymentId" PaymentId
                                    :> "refunds"
                                    :> Capture "id" RefundId
                                    :> DeleteNoContent '[HalJSON] NoContent
    -- ^Handler to cancel a refund by its identifier for a specific payment. See https://docs.mollie.com/reference/v2/refunds-api/cancel-refund
    } deriving Generic

{-|
  Helper to create a minimal new refund. Defaults to refunding the total amount for the targeted payment.
-}
newRefund :: NewRefund
newRefund = def
