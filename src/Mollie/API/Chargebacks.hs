{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Chargebacks
    ( Chargeback (..)
    , ChargebackId
    , chargebacksPath
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

import           Control.Lens        (makeFieldsNoPrefix)
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import           Data.Monoid
import qualified Data.Text           as Text
import qualified Data.Time           as Time
import           Mollie.API.Internal
import qualified Mollie.API.Payments as Payments
import           Mollie.API.Types

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

{-|
  Chargeback resource's path, relative to API's versioned URL.
-}
chargebacksPath :: Text.Text
chargebacksPath = "chargebacks"

{-|
  Handler to get a list of chargebacks. Because the list endpoint is paginated, this handler requires an offset and a limit. The maximum amount of chargebacks returned with a single call is 250.

  For more information see: https://mollie.com/en/docs/reference/chargebacks/list
-}
getChargebacks :: [QueryParam] -- ^ query params to pass to the list API
               -> Mollie (Either ResponseError (List Chargeback))
getChargebacks queryParams = get path
    where
        path = chargebacksPath <> toText queryParams

{-|
  Helper handler to get a list of chargebacks of a single payment. Because the list endpoint is paginated, this handler requires an offset and a limit. The maximum amount of chargebacks returned with a single call is 250.

  For more information see: https://mollie.com/en/docs/reference/chargebacks/list
-}
getPaymentChargebacks :: [QueryParam] -- ^ query params to pass to the list API
                      -> PaymentId -- ^ payment id
                      -> Mollie (Either ResponseError (List Chargeback))
getPaymentChargebacks queryParams paymentId_ = get path
    where
        path = chargebacksPath <> toText params
        params = (QueryParam "paymentId" paymentId_) : queryParams

{-|
  Handler to get a chargeback of a payment by its identifier.

  For more information see: https://mollie.com/en/docs/reference/chargebacks/get.
-}
getChargeback :: PaymentId -- ^ payment id
              -> ChargebackId -- ^ chargeback id
              -> Mollie (Either ResponseError Chargeback)
getChargeback paymentId_ chargebackId_ = get path
    where
        path = Text.intercalate "/" [Payments.paymentsPath, paymentId_, chargebacksPath, chargebackId_]
