{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}

{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Payments
    ( paymentsPath
    , newPayment
    , newRecurringPayment
    , createPayment
    , getPayment
    , getPayments
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

import           Control.Lens        (makeFieldsNoPrefix, (&), (.~))
import           Data.Aeson          ((.!=), (.:), (.:?))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Aeson.Types    as Aeson
import           Data.Default        (Default, def)
import           Data.Monoid
import qualified Data.Text           as Text
import qualified Data.Time           as Time
import           Mollie.API.Internal
import           Mollie.API.Methods  (PaymentMethod (..))
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP

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
    { _amount            :: Amount
    -- ^Set the amount to charge. Minimum based on available payment methods.
    , _description       :: Text.Text
    -- ^Set the description. Will be shown on card or bank statement.
    , _redirectUrl       :: Maybe Text.Text
    -- ^Set the url the customer will be redirected to after the payment process.
    , _webhookUrl        :: Maybe Text.Text
    -- ^Set a specific webhook for this payment.
    , _method            :: Maybe PaymentMethod
    -- ^Set a specific payment method for this payment. The customer will not have a choice when this is set.
    , _metadata          :: Maybe Aeson.Value
    -- ^Set any additional data in JSON format.
    , _locale            :: Maybe Text.Text
    -- ^Force the payment screen language.
    , _sequenceType      :: Maybe SequenceType
    -- ^Set the equence type, default to `Oneoff`. For more information see: https://www.mollie.com/en/docs/reference/customers/create-payment.
    , _customerId        :: Maybe CustomerId
    -- ^Set a customer account for this payment.
    , _mandateId         :: Maybe MandateId
    -- ^Set the ID of a specific Mandate. May be supplied to indicate which of the consumer’s accounts should be credited.
    -- IDEAL fields
    , _issuer            :: Maybe Text.Text
    -- CREDIT CARD
    , _billingAddress    :: Maybe Address
    -- ^Set card holder's address. This is to improve the credit card fraude protection.
    -- CREDIT CARD & PAYPAL
    , _shippingAddress   :: Maybe Address
    -- ^Set the shipping address. This is to improve fraude protection.
    -- BANK TRANSFER
    , _billingEmail      :: Maybe Text.Text
    -- ^Set the billing email address. Billing instructions will be send immediately when the payment is created. When no locale is set the email will be sent in English.
    , _dueDate           :: Maybe Text.Text
    -- ^Set the date this payment should expire, in `YYYY-MM-DD` format. Minimum date is tomorrow and maximum is 100 days from now.

    -- SEPA DIRECT DEBIT
    , _consumerName      :: Maybe Text.Text
    -- ^Set the beneficiary name of the account holder.
    , _consumerAccount   :: Maybe Text.Text
    -- ^Set the account holders IBAN.
    -- PAYSAFECARD
    , _customerReference :: Maybe Text.Text
    -- ^Set an identifier for the customer.
    }
    deriving (Show)

instance Default NewPayment where
    def = NewPayment
        { _amount = def
        , _description = mempty
        , _redirectUrl = def
        , _webhookUrl = def
        , _method = def
        , _metadata = def
        , _locale = def
        , _sequenceType = def
        , _customerId = def
        , _mandateId = def
        , _issuer = def
        , _billingAddress = def
        , _shippingAddress = def
        , _billingEmail = def
        , _dueDate = def
        , _consumerName = def
        , _consumerAccount = def
        , _customerReference = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        , Aeson.omitNothingFields  = True
        }
    ''NewPayment)

makeFieldsNoPrefix ''NewPayment

{-|
  Representation of a payment made with Mollie.

  Note that the amounts are curently returned as text because Mollie does not return them as valid json numbers.

  For more information see: https://www.mollie.com/nl/docs/reference/payments/get.
-}
data Payment = Payment
    { _id               :: PaymentId
    -- ^Mollie's reference to the payment resource.
    , _mode             :: Mode
    -- ^The mode used to create this payment.
    , _createdAt        :: Time.UTCTime
    -- ^The date on which the payment was created.
    , _status           :: PaymentStatus
    -- ^The current status.
    , _isCancelable     :: Bool
    -- ^Whether or not the payment can be canceled.
    , _paidAt           :: Maybe Time.UTCTime
    -- ^The date on which the payment was paid.
    , _canceledAt       :: Maybe Time.UTCTime
    -- ^The date on which the payment was canceled.
    , _expiredAt        :: Maybe Time.UTCTime
    -- ^The date on which the payment expired.
    , _failedAt         :: Maybe Time.UTCTime
    -- ^The date on which the payment failed.
    , _amount           :: Amount
    -- ^The amount charged for this payment.
    , _amountRefunded   :: Maybe Amount
    -- ^The amount which has been refunded.
    , _amountRemaining  :: Maybe Amount
    -- ^The amount which remained after refunding.
    , _description      :: Text.Text
    -- ^The payment description, as show on the bank or card statement.
    , _redirectUrl      :: Maybe Text.Text
    -- ^The URL your customer will be redirected to after completing or canceling the payment process
    , _webhookUrl       :: Maybe Text.Text
    -- ^The URL Mollie will call as soon an important status change takes place.
    , _method           :: Maybe PaymentMethod
    -- ^The payment method used.
    , _metadata         :: Maybe Aeson.Value
    -- ^Custom privided metadata.
    , _locale           :: Maybe Text.Text
    -- ^The language used during checkout.
    , _countryCode      :: Maybe Text.Text
    -- ^This optional field contains your customer’s ISO 3166-1 alpha-2 country code, detected during checkout.
    , _profileId        :: Text.Text
    -- ^Identifier for the profile this payment was created on.
    , _settlementAmount :: Maybe Amount
    -- ^The amount that will be settled to your account.
    , _settlementId     :: Maybe Text.Text
    -- ^The identifier referring to the settlement this payment was settled with.
    , _customerId       :: Maybe CustomerId
    -- ^Identifier for the customer this payment was created for.
    , _sequenceType     :: Maybe SequenceType
    -- ^Indicates which type of payment this is in a recurring sequence. Set to oneoff by default.
    , _mandateId        :: Maybe MandateId
    -- ^Identifier for the mandate used for this payment if it's recurring.
    , _subscriptionId   :: Maybe SubscriptionId
    -- ^Identifier for the subscription used for this payment.
    -- TODO: Add payment specific details, see: https://www.mollie.com/nl/docs/reference/payments/get
    , _details          :: Maybe Aeson.Object
    }
    deriving (Show)

instance Aeson.FromJSON Payment where
    parseJSON (Aeson.Object o) = do
        _id <- o .: "id"
        _mode <- o .: "mode"
        _createdAt <- o .: "createdAt"
        _status <- o .: "status"
        _paidAt <- o .:? "paidAt"
        _isCancelable <- o .:? "isCancelable" .!= False
        _canceledAt <- o .:? "canceledAt"
        _expiredAt <- o .:? "expiredAt"
        _failedAt <- o .:? "failedAt"
        _amount <- o .: "amount"
        _amountRefunded <- o .:? "amountRefunded"
        _amountRemaining <- o .:? "amountRemaining"
        _description <- o .: "description"
        _redirectUrl <- o .:? "redirectUrl"
        _webhookUrl <- o .:? "webhookUrl"
        _method <- o .:? "method"
        _metadata <- o .: "metadata"
        _locale <- o .:? "locale"
        _countryCode <- o .:? "countryCode"
        _profileId <- o .: "profileId"
        _settlementAmount <- o .:? "settlementAmount"
        _settlementId <- o .:? "settlementId"
        _customerId <- o .:? "customerId"
        _sequenceType <- o .:? "sequenceType"
        _mandateId <- o .:? "mandateId"
        _subscriptionId <- o .:? "subscriptionId"
        _details <- o .:? "details"

        return Payment{..}
    parseJSON invalid = Aeson.typeMismatch "Payment" invalid

makeFieldsNoPrefix ''Payment

{-|
  Payment resource's path, relative to API's versioned url or to a customer resource url.
-}
paymentsPath :: Text.Text
paymentsPath = "payments"

{-|
  Helper to create a minimal new payment for normal use.
-}
newPayment :: Double -- ^ amount
           -> Text.Text -- ^ description
           -> Text.Text -- ^ redirectUrl
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
newRecurringPayment :: Double -- ^ amount
                    -> Text.Text -- ^ description
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

{-|
  Handler to create a new payment.

  For more information see: https://www.mollie.com/en/docs/reference/payments/create.
-}
createPayment :: NewPayment -> Mollie (Either ResponseError Payment)
createPayment newPayment =
    decodeResult <$> send HTTP.methodPost path newPayment
    where
        path = paymentsPath

{-|
  Handler to get a payment by its identifier.

  For more information see: https://www.mollie.com/en/docs/reference/payments/get.
-}
getPayment :: Text.Text -- ^ paymentId
           -> Mollie (Either ResponseError Payment)
getPayment paymentId = get path
    where
        path = Text.intercalate "/" [paymentsPath, paymentId]

{-|
  Handler to get a list of payment. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payments returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/payments/list.
-}
getPayments :: [QueryParam] -- ^ queryParams
            -> Mollie (Either ResponseError (List Payment))
getPayments queryParams = get path
    where
        path = paymentsPath <> toText queryParams
