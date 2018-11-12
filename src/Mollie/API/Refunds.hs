{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Refunds
    ( RefundAPI
    , getRefunds
    , newRefund
    , createPaymentRefund
    , getPaymentRefund
    , cancelPaymentRefund
    , getPaymentRefunds
    , RefundStatus (..)
    , Refund (..)
    -- Lens getters
    , Mollie.API.Refunds.id
    , amount
    , description
    , settlementAmount
    , status
    , paymentId
    , createdAt
    ) where

import           Control.Lens        (makeFieldsNoPrefix)
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import           Data.Default        (Default, def)
import           Data.Monoid
import           Data.Proxy          (Proxy (..))
import qualified Data.Text           as Text
import qualified Data.Time           as Time
import           GHC.Generics        (Generic)
import           Mollie.API.Internal
import qualified Mollie.API.Payments as Payments
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP
import           Servant.API
import           Servant.API.Generic
import           Servant.Client

{-|
  Structure to request a refund.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/create.
-}
data NewRefund = NewRefund
    { _amount      :: Maybe Amount
    -- ^The amount to refund. For some payments, it can be up to €25.00 more than the original transaction amount.
    , _description :: Maybe Text.Text
    -- ^Set the description. Will be shown on card or bank statement.
    }
    deriving (Show)

instance Default NewRefund where
    def = NewRefund
        { _amount = def
        , _description = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''NewRefund)

makeFieldsNoPrefix ''NewRefund

{-|
  All possible statusses a refund could be assigned.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/get.
-}
data RefundStatus
    = RefundQueued
    -- ^The refund will be processed once you have enough balance. You can still cancel this refund.
    | RefundPending
    -- ^The refund will be processed soon (usually the next business day). You can still cancel this refund.
    | RefundProcessing
    -- ^The refund is being processed. Cancellation is no longer possible.
    | RefundRefunded
    -- ^The refund has been paid out to your customer.
    | RefundFailed
    -- ^The refund has failed during processing.
    deriving (Read, Show, Eq)

instance ToText RefundStatus where
    toText = Text.pack . Aeson.camelTo2 '_' . drop 6 . show

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_' . drop 6
        }
    ''RefundStatus)

{-|
  Representation of a refund made with Mollie.

  Note that the amount is curently returned as text because Mollie does not return it as a valid json number.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/get.
-}
data Refund = Refund
    { _id               :: Text.Text
    -- ^Mollies reference to the refund.
    , _amount           :: Amount
    -- ^The amount refunded to your customer with this refund.
    , _settlementAmount :: Maybe Amount
    -- ^This optional field will contain the amount that will be deducted from your account balance.
    , _description      :: Text.Text
    -- ^The description of the refund that may be shown to your customer.
    , _status           :: RefundStatus
    -- ^The status in which this refund currently is.
    , _paymentId        :: PaymentId
    -- ^The unique identifier of the payment this refund was created for.
    , _createdAt        :: Time.UTCTime
    -- ^The date and time the refund was issued.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Refund)

makeFieldsNoPrefix ''Refund

data RefundAPI route = RefundAPI
    { getRefunds          :: route :- "refunds" :> Get '[HalJSON] (List Refund)
    , getPaymentRefunds   :: route :- "payments" :> Capture "paymentId" PaymentId :> "refunds" :> Get '[HalJSON] (List Refund)
    , createPaymentRefund :: route :- "payments" :> Capture "paymentId" PaymentId :> "refunds" :> ReqBody '[JSON] NewRefund :> Post '[HalJSON] Refund
    , getPaymentRefund    :: route :- "payments" :> Capture "paymentId" PaymentId :> "refunds" :> Capture "id" RefundId :> Get '[HalJSON] Refund
    , cancelPaymentRefund :: route :- "payments" :> Capture "paymentId" PaymentId :> "refunds" :> Capture "id" RefundId :> DeleteNoContent '[HalJSON] NoContent
    } deriving Generic

{-|
  Helper to create a minimal new refund. Defaults to refunding the total amount for the targetted payment.
-}
newRefund :: NewRefund
newRefund = def
