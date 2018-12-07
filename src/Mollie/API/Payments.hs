{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeOperators          #-}

{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Payments
    ( PaymentAPI
    , newPayment
    , newRecurringPayment
    , createPayment
    , getPayment
    , getPayments
    , getPaymentsPaginated
    , PaymentStatus (..)
    , PaymentMethod (..)
    , SequenceType (..)
    , NewPayment (..)
    , PaymentId
    , Payment (..)
    -- Lens getters
    , Mollie.API.Payments.id
    , mode
    , createdAt
    , status
    , isCancelable
    , paidAt
    , canceledAt
    , expiredAt
    , failedAt
    , amount
    , amountRefunded
    , amountRemaining
    , description
    , redirectUrl
    , webhookUrl
    , method
    , metadata
    , locale
    , countryCode
    , profileId
    , settlementAmount
    , settlementId
    , customerId
    , sequenceType
    , mandateId
    , subscriptionId
    , details
    , issuer
    , billingAddress
    , shippingAddress
    , billingEmail
    , dueDate
    , consumerName
    , consumerAccount
    , customerReference
    ) where

import           Control.Lens        (makeFields, (&), (.~))
import           Data.Aeson          ((.!=), (.:), (.:?))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Aeson.Types    as Aeson
import           Data.Default        (Default, def)
import qualified Data.Text           as Text
import qualified Data.Time           as Time
import           GHC.Generics        (Generic)
import           Mollie.API.Helpers
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Methods  (PaymentMethod (..))
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

{-|
  All possible statusses which can be assigned to a payment.
  When an important status changes occurs Mollie will notify
  the application by requesting the configured Webhook. Note
  that some changes will never be known to the application.

  For more information see: https://www.mollie.com/en/docs/status.
-}
data PaymentStatus
    = PaymentOpen
    -- ^Payment has been created. This is the initial status.
    | PaymentCanceled
    -- ^Customer has canceled the payment.
    | PaymentPending
    -- ^The payment process has been started. No notification.
    | PaymentExpired
    -- ^The payment has expired. Some payment methods (like `banktransfer`) might need a few days to process.
    | PaymentFailed
    -- ^The payment can't be completed.
    | PaymentPaid
    -- ^The payment was successful. This is the success status.
    deriving (Read, Show, Eq)

instance ToText PaymentStatus where
    toText = Text.pack . Aeson.camelTo2 '_' . drop 7 . show

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_' . drop 7
        }
    ''PaymentStatus)

{-|
  All available recurring types.
-}
data SequenceType
    = First
    | Recurring
    | Oneoff
    deriving (Read, Show, Eq)

instance ToText SequenceType where
    toText = Text.pack . Aeson.camelTo2 '_' . show

$(Aeson.deriveJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_'
        }
    ''SequenceType)

{-|
  Structure to request a new payment with.

  For more information see: https://www.mollie.com/en/docs/reference/payments/create.
-}
data NewPayment = NewPayment
    { _newPaymentAmount            :: Amount
    -- ^Set the amount to charge. Minimum based on available payment methods.
    , _newPaymentDescription       :: Text.Text
    -- ^Set the description. Will be shown on card or bank statement.
    , _newPaymentRedirectUrl       :: Maybe Text.Text
    -- ^Set the url the customer will be redirected to after the payment process.
    , _newPaymentWebhookUrl        :: Maybe Text.Text
    -- ^Set a specific webhook for this payment.
    , _newPaymentMethod            :: Maybe PaymentMethod
    -- ^Set a specific payment method for this payment. The customer will not have a choice when this is set.
    , _newPaymentMetadata          :: Maybe Aeson.Value
    -- ^Set any additional data in JSON format.
    , _newPaymentLocale            :: Maybe Text.Text
    -- ^Force the payment screen language.
    , _newPaymentSequenceType      :: Maybe SequenceType
    -- ^Set the equence type, default to `Oneoff`. For more information see: https://www.mollie.com/en/docs/reference/customers/create-payment.
    , _newPaymentCustomerId        :: Maybe CustomerId
    -- ^Set a customer account for this payment.
    , _newPaymentMandateId         :: Maybe MandateId
    -- ^Set the ID of a specific Mandate. May be supplied to indicate which of the consumer’s accounts should be credited.
    -- IDEAL fields
    , _newPaymentIssuer            :: Maybe Text.Text
    -- CREDIT CARD
    , _newPaymentBillingAddress    :: Maybe Address
    -- ^Set card holder's address. This is to improve the credit card fraude protection.
    -- CREDIT CARD & PAYPAL
    , _newPaymentShippingAddress   :: Maybe Address
    -- ^Set the shipping address. This is to improve fraude protection.
    -- BANK TRANSFER
    , _newPaymentBillingEmail      :: Maybe Text.Text
    -- ^Set the billing email address. Billing instructions will be send immediately when the payment is created. When no locale is set the email will be sent in English.
    , _newPaymentDueDate           :: Maybe Text.Text
    -- ^Set the date this payment should expire, in `YYYY-MM-DD` format. Minimum date is tomorrow and maximum is 100 days from now.

    -- SEPA DIRECT DEBIT
    , _newPaymentConsumerName      :: Maybe Text.Text
    -- ^Set the beneficiary name of the account holder.
    , _newPaymentConsumerAccount   :: Maybe Text.Text
    -- ^Set the account holders IBAN.
    -- PAYSAFECARD
    , _newPaymentCustomerReference :: Maybe Text.Text
    -- ^Set an identifier for the customer.
    }
    deriving (Show)

instance Default NewPayment where
    def = NewPayment
        { _newPaymentAmount = def
        , _newPaymentDescription = mempty
        , _newPaymentRedirectUrl = def
        , _newPaymentWebhookUrl = def
        , _newPaymentMethod = def
        , _newPaymentMetadata = def
        , _newPaymentLocale = def
        , _newPaymentSequenceType = def
        , _newPaymentCustomerId = def
        , _newPaymentMandateId = def
        , _newPaymentIssuer = def
        , _newPaymentBillingAddress = def
        , _newPaymentShippingAddress = def
        , _newPaymentBillingEmail = def
        , _newPaymentDueDate = def
        , _newPaymentConsumerName = def
        , _newPaymentConsumerAccount = def
        , _newPaymentCustomerReference = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 11
        , Aeson.omitNothingFields  = True
        }
    ''NewPayment)

makeFields ''NewPayment

{-|
  Representation of a payment made with Mollie.

  Note that the amounts are curently returned as text because Mollie does not return them as valid json numbers.

  For more information see: https://www.mollie.com/nl/docs/reference/payments/get.
-}
data Payment = Payment
    { _paymentId               :: PaymentId
    -- ^Mollie's reference to the payment resource.
    , _paymentMode             :: Mode
    -- ^The mode used to create this payment.
    , _paymentCreatedAt        :: Time.UTCTime
    -- ^The date on which the payment was created.
    , _paymentStatus           :: PaymentStatus
    -- ^The current status.
    , _paymentIsCancelable     :: Bool
    -- ^Whether or not the payment can be canceled.
    , _paymentPaidAt           :: Maybe Time.UTCTime
    -- ^The date on which the payment was paid.
    , _paymentCanceledAt       :: Maybe Time.UTCTime
    -- ^The date on which the payment was canceled.
    , _paymentExpiredAt        :: Maybe Time.UTCTime
    -- ^The date on which the payment expired.
    , _paymentFailedAt         :: Maybe Time.UTCTime
    -- ^The date on which the payment failed.
    , _paymentAmount           :: Amount
    -- ^The amount charged for this payment.
    , _paymentAmountRefunded   :: Maybe Amount
    -- ^The amount which has been refunded.
    , _paymentAmountRemaining  :: Maybe Amount
    -- ^The amount which remained after refunding.
    , _paymentDescription      :: Text.Text
    -- ^The payment description, as show on the bank or card statement.
    , _paymentRedirectUrl      :: Maybe Text.Text
    -- ^The URL your customer will be redirected to after completing or canceling the payment process
    , _paymentWebhookUrl       :: Maybe Text.Text
    -- ^The URL Mollie will call as soon an important status change takes place.
    , _paymentMethod           :: Maybe PaymentMethod
    -- ^The payment method used.
    , _paymentMetadata         :: Maybe Aeson.Value
    -- ^Custom privided metadata.
    , _paymentLocale           :: Maybe Text.Text
    -- ^The language used during checkout.
    , _paymentCountryCode      :: Maybe Text.Text
    -- ^This optional field contains your customer’s ISO 3166-1 alpha-2 country code, detected during checkout.
    , _paymentProfileId        :: Text.Text
    -- ^Identifier for the profile this payment was created on.
    , _paymentSettlementAmount :: Maybe Amount
    -- ^The amount that will be settled to your account.
    , _paymentSettlementId     :: Maybe Text.Text
    -- ^The identifier referring to the settlement this payment was settled with.
    , _paymentCustomerId       :: Maybe CustomerId
    -- ^Identifier for the customer this payment was created for.
    , _paymentSequenceType     :: Maybe SequenceType
    -- ^Indicates which type of payment this is in a recurring sequence. Set to oneoff by default.
    , _paymentMandateId        :: Maybe MandateId
    -- ^Identifier for the mandate used for this payment if it's recurring.
    , _paymentSubscriptionId   :: Maybe SubscriptionId
    -- ^Identifier for the subscription used for this payment.
    -- TODO: Add payment specific details, see: https://www.mollie.com/nl/docs/reference/payments/get
    , _paymentDetails          :: Maybe Aeson.Object
    }
    deriving (Show)

instance Aeson.FromJSON Payment where
    parseJSON (Aeson.Object o) = do
        _paymentId <- o .: "id"
        _paymentMode <- o .: "mode"
        _paymentCreatedAt <- o .: "createdAt"
        _paymentStatus <- o .: "status"
        _paymentPaidAt <- o .:? "paidAt"
        _paymentIsCancelable <- o .:? "isCancelable" .!= False
        _paymentCanceledAt <- o .:? "canceledAt"
        _paymentExpiredAt <- o .:? "expiredAt"
        _paymentFailedAt <- o .:? "failedAt"
        _paymentAmount <- o .: "amount"
        _paymentAmountRefunded <- o .:? "amountRefunded"
        _paymentAmountRemaining <- o .:? "amountRemaining"
        _paymentDescription <- o .: "description"
        _paymentRedirectUrl <- o .:? "redirectUrl"
        _paymentWebhookUrl <- o .:? "webhookUrl"
        _paymentMethod <- o .:? "method"
        _paymentMetadata <- o .: "metadata"
        _paymentLocale <- o .:? "locale"
        _paymentCountryCode <- o .:? "countryCode"
        _paymentProfileId <- o .: "profileId"
        _paymentSettlementAmount <- o .:? "settlementAmount"
        _paymentSettlementId <- o .:? "settlementId"
        _paymentCustomerId <- o .:? "customerId"
        _paymentSequenceType <- o .:? "sequenceType"
        _paymentMandateId <- o .:? "mandateId"
        _paymentSubscriptionId <- o .:? "subscriptionId"
        _paymentDetails <- o .:? "details"

        return Payment{..}
    parseJSON invalid = Aeson.typeMismatch "Payment" invalid

makeFields ''Payment

{-|
  Helper to create a minimal new payment for normal use.
-}
newPayment :: Double -- ^ _amount
           -> Text.Text -- ^ _description
           -> Text.Text -- ^ _redirectUrl
           -> NewPayment
newPayment _amount _description _redirectUrl =
    (newRecurringPayment _amount _description)
      & redirectUrl .~ Just _redirectUrl
      & sequenceType .~ Just Oneoff

{-|
  Helper to create a minimal new payment for recurring use.

  A payment created with this helper should be sent with the
  `createCustomerPayment` from `Mollie.API.Customers` or have
  the customerId set.

  For a first recurring payment use `newPayment` and set the
  recurring type to `First`, because it needs a return url.
-}
newRecurringPayment :: Double -- ^ _amount
                    -> Text.Text -- ^ _description
                    -> NewPayment
newRecurringPayment _amount _description =
    def
      & amount .~ (defaultAmount _amount)
      & description .~ _description
      & redirectUrl .~ Nothing
      & webhookUrl .~ Nothing
      & method .~ Nothing
      & metadata .~ Nothing
      & locale .~ Nothing
      & sequenceType .~ Just Recurring
      & customerId .~ Nothing
      & mandateId .~ Nothing
      & issuer .~ Nothing
      & billingAddress .~ Nothing
      & shippingAddress .~ Nothing
      & billingEmail .~ Nothing
      & dueDate .~ Nothing
      & consumerName .~ Nothing
      & consumerAccount .~ Nothing
      & customerReference .~ Nothing

data PaymentAPI route = PaymentAPI
    { getPaymentsPaginated :: route :- "payments"
                              :> QueryParam "limit" Int
                              :> QueryParam "from" PaymentId
                              :> Get '[HalJSON] (List Payment)
    -- ^Handler to get a paginated list of payments. Offset the results by passing the last payment ID in the `from` query param. The payment with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/payments-api/list-payments
    , getPayments          :: route :- "payments"
                              :> Get '[HalJSON] (List Payment)
    -- ^Handler to get a paginated list of payments. Applies default pagination for newest 250 customers. See https://docs.mollie.com/reference/v2/payments-api/list-payments
    , createPayment        :: route :- "payments"
                              :> ReqBody '[JSON] NewPayment
                              :> Post '[HalJSON] Payment
    -- ^Handler to create a new payment. See https://docs.mollie.com/reference/v2/payments-api/create-payment
    , getPayment           :: route :- "payments"
                              :> Capture "id" PaymentId
                              :> Get '[HalJSON] Payment
    -- ^Handler to get a payment by its identifier. See https://docs.mollie.com/reference/v2/payments-api/create-payment
    } deriving Generic
