{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Types where

import qualified Control.Lens        as Lens
import qualified Control.Lens.TH     as Lens
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Aeson.Types    as Aeson
import qualified Data.Currency       as Currency
import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified Data.Time           as Time
import           GHC.Generics
import qualified Text.Printf         as Printf

{-|
  Helper class for when data is required to be transformed
  to Mollies format.
-}
class ToText a where
    toText :: a -> Text.Text

{-|
  In v2 endpoints, an amount object is always represented as follows:

  For more information see: https://docs.mollie.com/guides/common-data-types#amount-object
-}
data Amount = Amount
    { _currency :: Currency.Alpha
    -- ^An ISO 4217 currency code. The currencies supported depend on the payment methods that are enabled on your account.
    , _value    :: Text.Text
    -- ^A string containing the exact amount you want to charge in the given currency. Make sure to send the right amount of decimals
    } deriving (Show, Eq)

instance Default Amount where
    def = Amount
        { _currency = Currency.EUR
        , _value = mempty
        }

{-|
  Creates a Mollie amount given a Double
-}
defaultAmount :: Double -> Amount
defaultAmount x =
    def { _value = Text.pack $ Printf.printf "%.2f" x }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Amount)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Amount)

Lens.makeFieldsNoPrefix ''Amount

data Address = Address
    { _streetAndNumber  :: Text.Text
    -- ^The card holder’s street and street number
    , _streetAdditional :: Maybe Text.Text
    , _postalCode       :: Text.Text
    -- ^The card holder’s postal code
    , _city             :: Text.Text
    -- ^The card holder’s city
    , _region           :: Maybe Text.Text
    -- ^The card holder’s region. Sometimes required for Paypal payments.
    , _country          :: Text.Text
    -- ^The card holder’s country in ISO 3166-1 alpha-2 format
    } deriving (Show)

instance Default Address where
    def = Address
        { _streetAndNumber = mempty
        , _streetAdditional = mempty
        , _postalCode = mempty
        , _city = mempty
        , _region = mempty
        , _country = mempty
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Address)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Address)

Lens.makeFieldsNoPrefix ''Address

data Link = Link
    { _href :: Text.Text
    } deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Link)

Lens.makeFieldsNoPrefix ''Link

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
  All possible payment methods.
-}
data PaymentMethod
    = Ideal
    | Creditcard
    | Sofort
    | Banktransfer
    | Directdebit
    | Bancontact
    | Eps
    | Giropay
    | Kbc
    | Belfius
    | Paypal
    | Bitcoin
    | Podiumcadeaukaart
    | Paysafecard
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
                  [ Ideal, Creditcard, Sofort, Banktransfer
                  , Directdebit, Belfius, Paypal, Bitcoin, Podiumcadeaukaart
                  , Paysafecard
                  ]

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
    , _customerId        :: Maybe Text.Text
    -- ^Set a customer account for this payment.
    , _mandateId         :: Maybe Int
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
  Representation of a payment made with Mollie.

  Note that the amounts are curently returned as text because Mollie does not return them as valid json numbers.

  For more information see: https://www.mollie.com/nl/docs/reference/payments/get.
-}
data Payment = Payment
    { _id               :: Text.Text
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
    , _customerId       :: Maybe Text.Text
    -- ^Identifier for the customer this payment was created for.
    , _sequenceType     :: Maybe SequenceType
    -- ^Indicates which type of payment this is in a recurring sequence. Set to oneoff by default.
    , _mandateId        :: Maybe Text.Text
    -- ^Identifier for the mandate used for this payment if it's recurring.
    , _subscriptionId   :: Maybe Text.Text
    -- ^Identifier for the subscription used for this payment.
    -- TODO: Add payment specific details, see: https://www.mollie.com/nl/docs/reference/payments/get
    , _details          :: Maybe Aeson.Object
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Payment)

{-|
  Important links associated with List responses.
-}
data ListLinks = ListLinks
    { _self          :: Link
    -- ^The URL to the current set of objects..
    , _next          :: Maybe Link
    -- ^The previous set of objects, if available.
    , _previous      :: Maybe Link
    -- ^The next set of objects, if available.
    , _documentation :: Maybe Link
    -- ^URL to the documentation of the current endpoint.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''ListLinks)

Lens.makeFieldsNoPrefix ''ListLinks

{-|
  List response for any resource with metadata.

  For more information see: https://www.mollie.com/nl/docs/reference/payments/list.
-}
data List a = List
    { _count    :: Int
    -- ^The number of objects found in `_embedded`, which is either the requested number (with a maximum of 250) or the default number.
    , _embedded :: [a]
    -- ^The actual data you’re looking for.
    , _links    :: ListLinks
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

Lens.makeFieldsNoPrefix ''List

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
    , _paymentId        :: Text.Text
    -- ^The unique identifier of the payment this refund was created for.
    , _payment          :: Maybe Payment
    -- ^The payment this refund was made for.
    , _createdAt        :: Time.UTCTime
    -- ^The date and time the refund was issued.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Refund)

{-|
  Images associated with a payment method.
-}
data MethodImage = MethodImage
    { _size1x :: Text.Text
    -- ^Normal method icon, 32x24 pixels.
    , _size2x :: Text.Text
    -- ^Bigger method icon, 64x48px pixels.
    , _svg    :: Text.Text
    -- ^Vector icon, can scale to any size.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''MethodImage)

{-|
  Representation of a payment method available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/methods/get.
-}
data Method = Method
    { _id          :: PaymentMethod
    -- ^Mollies reference to the method.
    , _description :: Text.Text
    -- ^Full name of the method. This value changes based on requested locale.
    , _image       :: MethodImage
    -- ^Icons for this method.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Method)

{-|
  Representation of an issuer available at Mollie.

-}
data Issuer = Issuer
    { _id     :: Text.Text
    -- ^Mollies reference to the issuer.
    , _name   :: Text.Text
    -- ^The issuers full name.
    , _method :: PaymentMethod
    -- ^The payment method this issuer belongs to. Currently only Ideal is supported.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Issuer)

{-|
  Structure to request a new customer with.

  For more information see: https://www.mollie.com/en/docs/reference/customers/create.
-}
data NewCustomer = NewCustomer
    { _name     :: Maybe Text.Text
    -- ^Set the full name of the customer.
    , _email    :: Maybe Text.Text
    -- ^Set the email address.
    , _locale   :: Maybe Text.Text
    -- ^Set the language to use for this customer during checkout,
    , _metadata :: Maybe Aeson.Value
    -- ^Set any additional data in JSON format.
    }
    deriving (Show)

instance Default NewCustomer where
    def = NewCustomer
        { _name = def
        , _email = def
        , _locale = def
        , _metadata = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''NewCustomer)

{-|
  Representation of an customer available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/customers/get.
-}
data Customer = Customer
    { _id                  :: Text.Text
    -- ^Mollies reference to the customer.
    , _mode                :: Mode
    -- ^The mode in which this customer was created.
    , _name                :: Maybe Text.Text
    -- ^The customers full name.
    , _email               :: Maybe Text.Text
    -- ^The cusomters email address.
    , _locale              :: Maybe Text.Text
    -- ^The locale used for this customer during checkout.
    , _metadata            :: Maybe Aeson.Value
    -- ^Custom privided data for this customer.
    , _recentlyUsedMethods :: [PaymentMethod]
    -- ^The payment methods this customer recently used.
    , _createdAt           :: Time.UTCTime
    -- ^The creation date of this customer.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Customer)

{-|
  Structure to request a new mandate with.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/create.
-}
data NewMandate = NewMandate
    { _method           :: PaymentMethod
    -- ^Set the payment method of the mandate. Currently only directdebit is supported.
    , _consumerName     :: Text.Text
    -- ^Set the consumer's name.
    , _consumerAccount  :: Text.Text
    -- ^Set the consumer's IBAN.
    , _consumerBic      :: Maybe Text.Text
    -- ^Set the consumer's bank BIC/SWIFT code.
    , _signatureDate    :: Maybe Text.Text
    -- ^Set the date the mandate was signed in `YYYY-MM-DD` format.
    , _mandateReference :: Maybe Text.Text
    -- ^Set a custom reference to this mandate.
    }
    deriving (Show)

instance Default NewMandate where
    def = NewMandate
        { _method = Directdebit
        , _consumerName = mempty
        , _consumerAccount = mempty
        , _consumerBic = def
        , _signatureDate = def
        , _mandateReference = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''NewMandate)

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
    { _consumerName    :: Maybe Text.Text
    -- ^The direct debit account holder's name.
    , _consumerAccount :: Maybe Text.Text
    -- ^The direct debit account IBAN.
    , _consumerBic     :: Maybe Text.Text
    -- ^The direct debit account BIC.
    , _cardHolder      :: Maybe Text.Text
    -- ^The credit card holder's name.
    , _cardNumber      :: Maybe Text.Text
    -- ^The last 4 digits of the credit card number.
    , _cardLabel       :: Maybe Text.Text
    -- ^The credit card's label.
    , _cardFingerprint :: Maybe Text.Text
    -- ^Unique alphanumeric representation of a credit card. Usable to identify returning customers.
    , _cardExpiryDate  :: Maybe Text.Text
    -- ^The credit card's expiry date in `YYYY-MM-DD` format.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''MandateDetails)

{-|
  Representation of a mandate available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/get.
-}
data Mandate = Mandate
    { _id               :: Text.Text
    -- ^Mollies reference to the mandate.
    , _status           :: MandateStatus
    -- ^The status of the mandate.
    , _method           :: PaymentMethod
    -- ^The payment method of the mandate.
    , _details          :: Maybe MandateDetails
    -- ^The mandate details.
    , _mandateReference :: Maybe Text.Text
    -- ^The custom reference set for this mandate.
    , _signatureDate    :: Maybe Text.Text
    -- ^Set the date the mandate was signed in `YYYY-MM-DD` format.
    , _createdAt        :: Time.UTCTime
    -- ^The date on which this mandate was created.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Mandate)

{-|
  Structure to request a new subscription with.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/create.
-}
data NewSubscription = NewSubscription
    { _amount      :: Amount
    -- ^Set the amount you want to charge each subscription cycle.
    , _times       :: Maybe Int
    -- ^Set the total number of charges for the subscription to complete. Leave empty for ongoing subscriptions.
    , _interval    :: Text.Text
    -- ^Set the interval to wait between charges like `1 month(s)`, `2 weeks` or `14 days`.
    , _startDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format. This is the first day on which your customer will be charged. When this parameter is not provided, the current date will be used instead.
    , _description :: Text.Text
    -- ^Set the description which will be included in the payment description along with the carge date in `Y-m-d` format.
    , _method      :: Maybe PaymentMethod
    -- ^Force the payment method, leave empty to use one of the customers valid mandates.
    , _webhookUrl  :: Maybe Text.Text
    -- ^Set a webhook URL for all subscription payments.
    }
    deriving (Show)

instance Default NewSubscription where
    def = NewSubscription
        { _amount = def
        , _times = def
        , _interval = mempty
        , _startDate = def
        , _description = mempty
        , _method = def
        , _webhookUrl = def
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''NewSubscription)

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
    { _id          :: Text.Text
    -- ^Mollies reference to the subscription.
    , _mode        :: Mode
    -- ^The mode used to create this subscription
    , _createdAt   :: Time.UTCTime
    -- ^The date on which this subscription was created.
    , _status      :: SubscriptionStatus
    -- ^The subscriptions status.
    , _amount      :: Amount
    -- ^The amount charged with each payment for this subscription.
    , _times       :: Maybe Int
    -- ^The total number or charges for the subscription to complete.
    , _interval    :: Text.Text
    -- ^The interval to wait between charges.
    , _startDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format.
    , _description :: Text.Text
    -- ^The description for the payments made with this subscription.
    , _method      :: Maybe PaymentMethod
    -- ^The payment method used for this subscription.
    , _canceledAt  :: Maybe Time.UTCTime
    -- ^The date on which this subscription was canceled.
    , _webhookUrl  :: Maybe Text.Text
    -- ^The URL Mollie will call as soon a payment status change takes place.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Subscription)

data Chargeback = Chargeback
    { _id               :: Text.Text
    -- ^Mollies reference to the chargeback.
    , _amount           :: Amount
    -- ^The amount charged back by the consumer.
    , _settlementAmount :: Amount
    -- ^The amount that will be deducted from your account
    , _createdAt        :: Time.UTCTime
    -- ^The date and time the chargeback was issued.
    , _reversedAt       :: Maybe Time.UTCTime
    -- ^The date and time the chargeback was issued.
    , _paymentId        :: Text.Text
    -- ^The unique identifier of the payment this chargeback was issued for.
    , _payment          :: Maybe Payment
    -- ^The payment this chargeback was made for.
    }

{-|
  Error data representations.

  For more information see: https://www.mollie.com/en/docs/errors.
-}
data ErrorLinks = ErrorLinks
    { _documentation :: Link
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''ErrorLinks)

Lens.makeFieldsNoPrefix ''ErrorLinks

data Error = Error
    { _title  :: Text.Text
    , _detail :: Text.Text
    , _field  :: Maybe Text.Text
    , _links  :: Maybe ErrorLinks
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    ''Error)

Lens.makeFieldsNoPrefix ''Error


{-|
  Response errors which could happen when requesting resources from Mollie.
-}
data ResponseError
    = ClientError Int Error
    | ServerError Int
    | UnexpectedResponse Text.Text
    deriving (Show)
