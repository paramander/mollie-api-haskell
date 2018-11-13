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
    , getPaymentChargebacks
    , getChargeback
    -- Lens getters
    , Mollie.API.Chargebacks.id
    , amount
    , settlementAmount
    , createdAt
    , reversedAt
    , paymentId
    ) where

import           Control.Lens           (makeFieldsNoPrefix)
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.TH          as Aeson
import qualified Data.Time              as Time
import           GHC.Generics           (Generic)
import           Mollie.API.Internal    (HalJSON)
import qualified Mollie.API.Payments    as Payments
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

data Chargeback = Chargeback
    { _id               :: ChargebackId
    -- ^Mollies reference to the chargeback.
    , _amount           :: Amount
    -- ^The amount charged back by the consumer.
    , _settlementAmount :: Maybe Amount
    -- ^The amount that will be deducted from your account
    , _createdAt        :: Time.UTCTime
    -- ^The date and time the chargeback was issued.
    , _reversedAt       :: Maybe Time.UTCTime
    -- ^The date and time the chargeback was reversed.
    , _paymentId        :: PaymentId
    -- ^The unique identifier of the payment this chargeback was issued for.
    }

$(Aeson.deriveFromJSON
     Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
     ''Chargeback)

makeFieldsNoPrefix ''Chargeback

data ChargebackAPI route = ChargebackAPI
    { getChargebacks        :: route :- "chargebacks"
                               :> QueryParam "limit" Int
                               :> QueryParam "from" ChargebackId
                               :> Get '[HalJSON] (List Chargeback)
    , getPaymentChargebacks :: route :- "payments"
                               :> Capture "paymentId" Payments.PaymentId
                               :> "chargebacks"
                               :> QueryParam "limit" Int
                               :> QueryParam "from" ChargebackId
                               :> Get '[HalJSON] (List Chargeback)
    , getChargeback         :: route :- "payments"
                               :> Capture "paymentId" Payments.PaymentId
                               :> "chargebacks"
                               :> Capture "id" ChargebackId
                               :> Get '[HalJSON] Chargeback
    } deriving Generic
