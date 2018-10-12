{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Refunds
    ( refundsPath
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
import qualified Data.Text           as Text
import qualified Data.Time           as Time
import           Mollie.API.Internal
import qualified Mollie.API.Payments as Payments
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP

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

{-|
  Refund resource's path, relative to API's versioned url or to a Payment resource url.
-}
refundsPath :: Text.Text
refundsPath = "refunds"

{-|
  Handler to get a list of refunds. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of refunds returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/list-all.
-}
getRefunds :: Int -- ^ offset
           -> Int -- ^ count
           -> Mollie (Either ResponseError (List Refund))
getRefunds offset count = get path
    where
        path = refundsPath <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count

{-|
  Helper to create a minimal new refund. Defaults to refunding the total amount for the targetted payment.
-}
newRefund :: NewRefund
newRefund = def

{-|
  Handler to create a new refund for a specific payment.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/create.
-}
createPaymentRefund :: PaymentId -- ^ _paymentId
                    -> NewRefund
                    -> Mollie (Either ResponseError Refund)
createPaymentRefund _paymentId newRefund =
    decodeResult <$> send HTTP.methodPost path newRefund
    where
        path = Text.intercalate "/" [Payments.paymentsPath, _paymentId, refundsPath]

{-|
  Handler to get a list of refunds for a specific payment. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of refunds returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/list.
-}
getPaymentRefunds :: PaymentId -- ^ _paymentId
                  -> [QueryParam] -- ^ queryParams
                  -> Mollie (Either ResponseError (List Refund))
getPaymentRefunds _paymentId queryParams = get path
    where
        path = Text.intercalate "/" [Payments.paymentsPath, _paymentId, refundsPath] <> toText queryParams

{-|
  Handler to cancel a refund by its identifier for a specific payment.

  This request only works on refunds which have not yet started processing.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/delete.
-}
cancelPaymentRefund :: PaymentId -- ^ _paymentId
                    -> RefundId -- ^ _id
                    -> Mollie (Maybe ResponseError)
cancelPaymentRefund _paymentId _id =
    ignoreResult <$> delete path
    where
        path = Text.intercalate "/" [Payments.paymentsPath, _paymentId, refundsPath, _id]

{-|
  Handler to get a refund by its identifier for a specific payment.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/get.
-}
getPaymentRefund :: PaymentId -- ^ _paymentId
                 -> RefundId -- ^ _id
                 -> Mollie (Either ResponseError Refund)
getPaymentRefund _paymentId _id = get path
    where
        path = Text.intercalate "/" [Payments.paymentsPath, _paymentId, refundsPath, _id]
