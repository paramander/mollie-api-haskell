{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Types where

import qualified Control.Lens        as Lens
import           Control.Lens.TH     ()
import           Data.Aeson          ((.!=), (.:), (.:?))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Aeson.Types    as Aeson
import           Data.Char           (toLower)
import qualified Data.Currency       as Currency
import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified Data.Time           as Time
import           Mollie.API.Helpers  (lowerFirst)
import qualified Text.Printf         as Printf


{-|
  Helper class for when data is required to be transformed
  to Mollies format.
-}
class ToText a where
    toText :: a -> Text.Text

type CustomerId = Text.Text
type PaymentId = Text.Text
type RefundId = Text.Text
type SubscriptionId = Text.Text
type ChargebackId = Text.Text
type MandateId = Text.Text

{-|
  In v2 endpoints, an amount object is always represented as follows:

  For more information see: https://docs.mollie.com/guides/common-data-types#amount-object
-}
data Amount = Amount
    { _amountCurrency :: Currency.Alpha
    -- ^An ISO 4217 currency code. The currencies supported depend on the payment methods that are enabled on your account.
    , _amountValue    :: Text.Text
    -- ^A string containing the exact amount you want to charge in the given currency. Make sure to send the right amount of decimals
    } deriving (Show, Eq)

instance Default Amount where
    def = Amount
        { _amountCurrency = Currency.EUR
        , _amountValue = mempty
        }

{-|
  Creates a Mollie amount given a Double
-}
defaultAmount :: Double -> Amount
defaultAmount x =
    def { _amountValue = Text.pack $ Printf.printf "%.2f" x }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 7
        }
    ''Amount)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 7
        }
    ''Amount)

Lens.makeFields ''Amount

data Address = Address
    { _addressStreetAndNumber  :: Text.Text
    -- ^The card holder’s street and street number
    , _addressStreetAdditional :: Maybe Text.Text
    , _addressPostalCode       :: Text.Text
    -- ^The card holder’s postal code
    , _addressCity             :: Text.Text
    -- ^The card holder’s city
    , _addressRegion           :: Maybe Text.Text
    -- ^The card holder’s region. Sometimes required for Paypal payments.
    , _addressCountry          :: Text.Text
    -- ^The card holder’s country in ISO 3166-1 alpha-2 format
    } deriving (Show)

instance Default Address where
    def = Address
        { _addressStreetAndNumber = mempty
        , _addressStreetAdditional = mempty
        , _addressPostalCode = mempty
        , _addressCity = mempty
        , _addressRegion = mempty
        , _addressCountry = mempty
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 8
        }
    ''Address)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 8
        }
    ''Address)

Lens.makeFields ''Address

data Link = Link
    { _linkHref :: Text.Text
    } deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 5
        }
    ''Link)

Lens.makeFields ''Link

{-|
  All available API modes.
-}
data Mode
    = Live
    | Test
    deriving (Read, Show, Eq)

instance ToText Mode where
    toText = Text.pack . Aeson.camelTo2 '_' . show

$(Aeson.deriveJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_'
        }
    ''Mode)

{-|
  Important links associated with List responses.
-}
data ListLinks = ListLinks
    { _listLinksSelf          :: Link
    -- ^The URL to the current set of objects..
    , _listLinksNext          :: Maybe Link
    -- ^The previous set of objects, if available.
    , _listLinksPrevious      :: Maybe Link
    -- ^The next set of objects, if available.
    , _listLinksDocumentation :: Maybe Link
    -- ^URL to the documentation of the current endpoint.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 10
        }
    ''ListLinks)

Lens.makeFields ''ListLinks

{-|
  List response for any resource with metadata.

  For more information see: https://www.mollie.com/nl/docs/reference/payments/list.
-}
data List a = List
    { _listCount    :: Int
    -- ^The number of objects found in `_embedded`, which is either the requested number (with a maximum of 250) or the default number.
    , _listEmbedded :: [a]
    -- ^The actual data you’re looking for.
    , _listLinks    :: ListLinks
    -- ^Links to help navigate through the lists of objects.
    }
    deriving (Show)

instance Aeson.FromJSON a => Aeson.FromJSON (List a) where
    parseJSON (Aeson.Object v) = List
        <$> Aeson.parseField v "count"
        <*> fmap elems (Aeson.parseField v "_embedded")
        <*> Aeson.parseField v "_links"
        where elems :: HashMap.HashMap Text.Text [a] -> [a]
              elems = concat . HashMap.elems
    parseJSON invalid = Aeson.typeMismatch "Not a correct embed for a list" invalid

Lens.makeFields ''List

{-|
  Error data representations.

  For more information see: https://www.mollie.com/en/docs/errors.
-}
data ErrorLinks = ErrorLinks
    { _errorLinksDocumentation :: Link
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 11
        }
    ''ErrorLinks)

Lens.makeFields ''ErrorLinks

data Error = Error
    { _errorTitle  :: Text.Text
    , _errorDetail :: Text.Text
    , _errorField  :: Maybe Text.Text
    , _errorLinks  :: Maybe ErrorLinks
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 6
        }
    ''Error)

Lens.makeFields ''Error

{-|
  All possible payment methods.
-}
data PaymentMethod
    = Bancontact
    | Banktransfer
    | Belfius
    | Bitcoin
    | Creditcard
    | Directdebit
    | Eps
    | Giftcard
    | Giropay
    | Ideal
    | Inghomepay
    | Kbc
    | Paypal
    | Paysafecard
    | Sofort
    | NewPaymentMethod Text.Text -- When this shows up in a response from or is required for a request to Mollie contact package maintainer.
    deriving (Read, Show, Eq)

instance ToText PaymentMethod where
    toText (NewPaymentMethod text) = text
    toText a                       = Text.pack $ Aeson.camelTo2 '_' $ show a

instance Aeson.ToJSON PaymentMethod where
    toJSON = Aeson.String . toText

instance Aeson.FromJSON PaymentMethod where
    parseJSON val = case lookup val methods of
        Just method -> return method
        Nothing -> case val of
            (Aeson.String method) -> return $ NewPaymentMethod method
            invalid -> Aeson.typeMismatch "PaymentMethod" invalid
        where methods = map
                  (\method -> (Aeson.toJSON method, method))
                  [ Bancontact, Banktransfer, Belfius, Bitcoin
                  , Creditcard, Directdebit, Eps, Giftcard
                  , Giropay, Ideal, Inghomepay, Kbc
                  , Paypal, Paysafecard, Sofort
                  ]

{-|
  Response errors which could happen when requesting resources from Mollie.
-}
data ResponseError
    = ClientError Int Error
    | ServerError Int
    | UnexpectedResponse Text.Text
    deriving (Show)

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

Lens.makeFields ''Chargeback

{-|
  Structure to request a new customer with.

  For more information see: https://www.mollie.com/en/docs/reference/customers/create.
-}
data NewCustomer = NewCustomer
    { _newCustomerName     :: Maybe Text.Text
    -- ^Set the full name of the customer.
    , _newCustomerEmail    :: Maybe Text.Text
    -- ^Set the email address.
    , _newCustomerLocale   :: Maybe Text.Text
    -- ^Set the language to use for this customer during checkout,
    , _newCustomerMetadata :: Maybe Aeson.Value
    -- ^Set any additional data in JSON format.
    }
    deriving (Show)

instance Default NewCustomer where
    def = NewCustomer
        { _newCustomerName = def
        , _newCustomerEmail = def
        , _newCustomerLocale = def
        , _newCustomerMetadata = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 12
        }
    ''NewCustomer)

Lens.makeFields ''NewCustomer

{-|
  Representation of an customer available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/customers/get.
-}
data Customer = Customer
    { _customerId                  :: CustomerId
    -- ^Mollies reference to the customer.
    , _customerMode                :: Mode
    -- ^The mode in which this customer was created.
    , _customerName                :: Maybe Text.Text
    -- ^The customers full name.
    , _customerEmail               :: Maybe Text.Text
    -- ^The cusomters email address.
    , _customerLocale              :: Maybe Text.Text
    -- ^The locale used for this customer during checkout.
    , _customerMetadata            :: Maybe Aeson.Value
    -- ^Custom privided data for this customer.
    , _customerRecentlyUsedMethods :: [PaymentMethod]
    -- ^The payment methods this customer recently used.
    , _customerCreatedAt           :: Time.UTCTime
    -- ^The creation date of this customer.
    }
    deriving (Show)

instance Aeson.FromJSON Customer where
    parseJSON (Aeson.Object o) = do
        _customerId <- o .: "id"
        _customerMode <- o .: "mode"
        _customerName <- o .:? "name"
        _customerEmail <- o .:? "email"
        _customerLocale <- o .:? "locale"
        _customerMetadata <- o .:? "metadata"
        _customerRecentlyUsedMethods <- o .:? "recentlyUsedMethods" .!= []
        _customerCreatedAt <- o .: "createdAt"

        return Customer{..}
    parseJSON invalid = Aeson.typeMismatch "Customer" invalid

Lens.makeFields ''Customer

{-|
  Structure to request a new mandate with.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/create.
-}
data NewMandate = NewMandate
    { _newMandateMethod           :: PaymentMethod
    -- ^Set the payment method of the mandate. Currently only directdebit is supported.
    , _newMandateConsumerName     :: Text.Text
    -- ^Set the consumer's name.
    , _newMandateConsumerAccount  :: Text.Text
    -- ^Set the consumer's IBAN.
    , _newMandateConsumerBic      :: Maybe Text.Text
    -- ^Set the consumer's bank BIC/SWIFT code.
    , _newMandateSignatureDate    :: Maybe Text.Text
    -- ^Set the date the mandate was signed in `YYYY-MM-DD` format.
    , _newMandateMandateReference :: Maybe Text.Text
    -- ^Set a custom reference to this mandate.
    }
    deriving (Show)

instance Default NewMandate where
    def = NewMandate
        { _newMandateMethod = Directdebit
        , _newMandateConsumerName = mempty
        , _newMandateConsumerAccount = mempty
        , _newMandateConsumerBic = def
        , _newMandateSignatureDate = def
        , _newMandateMandateReference = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 11
        }
    ''NewMandate)

Lens.makeFields ''NewMandate

{-|
  All possible statusses for a Mandate.
-}
data MandateStatus
    = MandatePending -- TODO: Validate this state, from https://www.mollie.com/nl/docs/recurring.
    | MandateValid
    | MandateInvalid
    deriving (Read, Show, Eq)

instance ToText MandateStatus where
    toText = Text.pack . Aeson.camelTo2 '_' . drop 7 . show

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_' . drop 7
        }
    ''MandateStatus)

{-|
  Details which might be available on Mandates.
-}
data MandateDetails = MandateDetails
    { _mandateDetailsConsumerName    :: Maybe Text.Text
    -- ^The direct debit account holder's name.
    , _mandateDetailsConsumerAccount :: Maybe Text.Text
    -- ^The direct debit account IBAN.
    , _mandateDetailsConsumerBic     :: Maybe Text.Text
    -- ^The direct debit account BIC.
    , _mandateDetailsCardHolder      :: Maybe Text.Text
    -- ^The credit card holder's name.
    , _mandateDetailsCardNumber      :: Maybe Text.Text
    -- ^The last 4 digits of the credit card number.
    , _mandateDetailsCardLabel       :: Maybe Text.Text
    -- ^The credit card's label.
    , _mandateDetailsCardFingerprint :: Maybe Text.Text
    -- ^Unique alphanumeric representation of a credit card. Usable to identify returning customers.
    , _mandateDetailsCardExpiryDate  :: Maybe Text.Text
    -- ^The credit card's expiry date in `YYYY-MM-DD` format.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 15
        }
    ''MandateDetails)

Lens.makeFields ''MandateDetails

{-|
  Representation of a mandate available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/get.
-}
data Mandate = Mandate
    { _mandateId               :: MandateId
    -- ^Mollies reference to the mandate.
    , _mandateStatus           :: MandateStatus
    -- ^The status of the mandate.
    , _mandateMethod           :: PaymentMethod
    -- ^The payment method of the mandate.
    , _mandateDetails          :: Maybe MandateDetails
    -- ^The mandate details.
    , _mandateMandateReference :: Maybe Text.Text
    -- ^The custom reference set for this mandate.
    , _mandateSignatureDate    :: Maybe Text.Text
    -- ^Set the date the mandate was signed in `YYYY-MM-DD` format.
    , _mandateCreatedAt        :: Time.UTCTime
    -- ^The date on which this mandate was created.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 8
        }
    ''Mandate)

Lens.makeFields ''Mandate

{-|
  Images associated with a payment method.
-}
data MethodImage = MethodImage
    { _methodImageSize1x :: Text.Text
    -- ^Normal method icon, 32x24 pixels.
    , _methodImageSize2x :: Text.Text
    -- ^Bigger method icon, 64x48px pixels.
    , _methodImageSvg    :: Text.Text
    -- ^Vector icon, can scale to any size.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 12
        }
    ''MethodImage)

Lens.makeFields ''MethodImage

{-|
  Representation of a payment method available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/methods/get.
-}
data Method = Method
    { _methodId          :: PaymentMethod
    -- ^Mollies reference to the method.
    , _methodDescription :: Text.Text
    -- ^Full name of the method. This value changes based on requested locale.
    , _methodImage       :: MethodImage
    -- ^Icons for this method.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 7
        }
    ''Method)

Lens.makeFields ''Method

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

Lens.makeFields ''NewPayment

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
    , _paymentMollieUrl        :: Maybe Text.Text
    -- ^The payment screen URL your customer should visit to make the payment. This is where you should redirect the customer to.
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
        _paymentMollieUrl <- fmap (fmap _linkHref) ((o .: "_links") >>= (.:? "checkout"))

        return Payment{..}
    parseJSON invalid = Aeson.typeMismatch "Payment" invalid

Lens.makeFields ''Payment

{-|
  Structure to request a refund.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/create.
-}
data NewRefund = NewRefund
    { _newRefundAmount      :: Maybe Amount
    -- ^The amount to refund. For some payments, it can be up to €25.00 more than the original transaction amount.
    , _newRefundDescription :: Maybe Text.Text
    -- ^Set the description. Will be shown on card or bank statement.
    }
    deriving (Show)

instance Default NewRefund where
    def = NewRefund
        { _newRefundAmount = def
        , _newRefundDescription = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 10
        }
    ''NewRefund)

Lens.makeFields ''NewRefund

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
    { _refundId               :: Text.Text
    -- ^Mollies reference to the refund.
    , _refundAmount           :: Amount
    -- ^The amount refunded to your customer with this refund.
    , _refundSettlementAmount :: Maybe Amount
    -- ^This optional field will contain the amount that will be deducted from your account balance.
    , _refundDescription      :: Text.Text
    -- ^The description of the refund that may be shown to your customer.
    , _refundStatus           :: RefundStatus
    -- ^The status in which this refund currently is.
    , _refundPaymentId        :: PaymentId
    -- ^The unique identifier of the payment this refund was created for.
    , _refundCreatedAt        :: Time.UTCTime
    -- ^The date and time the refund was issued.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 7
        }
    ''Refund)

Lens.makeFields ''Refund

{-|
  Structure to request a new subscription with.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/create.
-}
data NewSubscription = NewSubscription
    { _newSubscriptionAmount      :: Amount
    -- ^Set the amount you want to charge each subscription cycle.
    , _newSubscriptionTimes       :: Maybe Int
    -- ^Set the total number of charges for the subscription to complete. Leave empty for ongoing subscriptions.
    , _newSubscriptionInterval    :: Text.Text
    -- ^Set the interval to wait between charges like `1 month(s)`, `2 weeks` or `14 days`.
    , _newSubscriptionStartDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format. This is the first day on which your customer will be charged. When this parameter is not provided, the current date will be used instead.
    , _newSubscriptionDescription :: Text.Text
    -- ^Set the description which will be included in the payment description along with the carge date in `Y-m-d` format.
    , _newSubscriptionMethod      :: Maybe PaymentMethod
    -- ^Force the payment method, leave empty to use one of the customers valid mandates.
    , _newSubscriptionWebhookUrl  :: Maybe Text.Text
    -- ^Set a webhook URL for all subscription payments.
    }
    deriving (Show)

instance Default NewSubscription where
    def = NewSubscription
        { _newSubscriptionAmount = def
        , _newSubscriptionTimes = def
        , _newSubscriptionInterval = mempty
        , _newSubscriptionStartDate = def
        , _newSubscriptionDescription = mempty
        , _newSubscriptionMethod = def
        , _newSubscriptionWebhookUrl = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 16
        }
    ''NewSubscription)

Lens.makeFields ''NewSubscription

{-|
  All possible statusses a subscription could be assigned.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/get.
-}
data SubscriptionStatus
    = SubscriptionPending
    | SubscriptionActive
    | SubscriptionCancelled
    | SubscriptionSuspended
    | SubscriptionCompleted
    deriving (Read, Show, Eq)

instance ToText SubscriptionStatus where
    toText = Text.pack . Aeson.camelTo2 '_' . drop 12 . show

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_' . drop 12
        }
    ''SubscriptionStatus)

{-|
  Representation of a subscription available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/get.
-}
data Subscription = Subscription
    { _subscriptionId          :: Text.Text
    -- ^Mollies reference to the subscription.
    , _subscriptionMode        :: Mode
    -- ^The mode used to create this subscription
    , _subscriptionCreatedAt   :: Time.UTCTime
    -- ^The date on which this subscription was created.
    , _subscriptionStatus      :: SubscriptionStatus
    -- ^The subscriptions status.
    , _subscriptionAmount      :: Amount
    -- ^The amount charged with each payment for this subscription.
    , _subscriptionTimes       :: Maybe Int
    -- ^The total number or charges for the subscription to complete.
    , _subscriptionInterval    :: Text.Text
    -- ^The interval to wait between charges.
    , _subscriptionStartDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format.
    , _subscriptionDescription :: Text.Text
    -- ^The description for the payments made with this subscription.
    , _subscriptionMethod      :: Maybe PaymentMethod
    -- ^The payment method used for this subscription.
    , _subscriptionCanceledAt  :: Maybe Time.UTCTime
    -- ^The date on which this subscription was canceled.
    , _subscriptionWebhookUrl  :: Maybe Text.Text
    -- ^The URL Mollie will call as soon a payment status change takes place.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = lowerFirst . drop 13
        }
    ''Subscription)

Lens.makeFields ''Subscription
