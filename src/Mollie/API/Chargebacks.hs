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
    --, chargebackApi
    , Chargeback (..)
    , ChargebackId
    , getChargebacks
    , getChargebacksPaginated
    , getChargeback
    -- Lens getters
    , Mollie.API.Chargebacks.id
    , amount
    , settlementAmount
    , createdAt
    , reversedAt
    , paymentId
    ) where

import           Control.Lens        (makeFields)
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Time           as Time
import           GHC.Generics        (Generic)
import           Mollie.API.Helpers
import           Mollie.API.Internal (HalJSON)
import qualified Mollie.API.Payments as Payments
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

data Chargeback = Chargeback
    { _chargebackId               :: ChargebackId
    -- ^Mollies reference to the chargeback.
    , _chargebackAmount           :: Amount
    -- ^The amount charged back by the consumer.
    , _chargebackSettlementAmount :: Maybe Amount
    -- ^The amount that will be deducted from your account
    , _chargebackCreatedAt        :: Time.UTCTime
    -- ^The date and time the chargeback was issued.
    , _chargebackReversedAt       :: Maybe Time.UTCTime
    -- ^The date and time the chargeback was reversed.
    , _chargebackPaymentId        :: PaymentId
    -- ^The unique identifier of the payment this chargeback was issued for.
    }

$(Aeson.deriveFromJSON
     Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 11
        }
     ''Chargeback)

makeFields ''Chargeback

data ChargebackAPI route = ChargebackAPI
    { getChargebacksPaginated        :: route :- "chargebacks"
                                        :> QueryParam "limit" Int
                                        :> QueryParam "from" ChargebackId
                                        :> Get '[HalJSON] (List Chargeback)
    -- ^Handler to get a paginated list of chargebacks. Offset the results by passing the last chargeback ID in the `from` query param. The chargeback with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/chargebacks-api/list-chargebacks
    , getChargebacks                 :: route :- "chargebacks"
                                        :> Get '[HalJSON] (List Chargeback)
    -- ^Handler to get a paginated list of chargebacks. Applies default pagination for newest 250 chargebacks. See https://docs.mollie.com/reference/v2/chargebacks-api/list-chargebacks
    , getChargeback                  :: route :- "payments"
                                        :> Capture "paymentId" Payments.PaymentId
                                        :> "chargebacks"
                                        :> Capture "id" ChargebackId
                                        :> Get '[HalJSON] Chargeback
    -- ^Handler to get a specific chargeback for a specific payment. See https://docs.mollie.com/reference/v2/chargebacks-api/get-chargeback
    } deriving Generic
