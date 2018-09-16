{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Mollie.API.Types where

import Debug.Trace
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Currency        as Currency
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text            as Text
import qualified Data.Time            as Time
import           GHC.Generics
import qualified Text.Printf          as Printf

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
    { amount_currency :: Currency.Alpha
    -- ^An ISO 4217 currency code. The currencies supported depend on the payment methods that are enabled on your account.
    , amount_value    :: Text.Text
    -- ^A string containing the exact amount you want to charge in the given currency. Make sure to send the right amount of decimals
    } deriving (Show, Eq)

{-|
  Creates a Mollie amount given a Double
-}
defaultAmount :: Double -> Amount
defaultAmount a =
    Amount
        { amount_currency = Currency.EUR
        , amount_value = Text.pack $ Printf.printf "%.2f" a
        }

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Amount)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Amount)

data Address = Address
    { address_streetAndNumber  :: Text.Text
    -- ^The card holder’s street and street number
    , address_streetAdditional :: Maybe Text.Text
    , address_postalCode       :: Text.Text
    -- ^The card holder’s postal code
    , address_city             :: Text.Text
    -- ^The card holder’s city
    , address_region           :: Maybe Text.Text
    -- ^The card holder’s region. Sometimes required for Paypal payments.
    , address_country          :: Text.Text
    -- ^The card holder’s country in ISO 3166-1 alpha-2 format
    } deriving (Show)

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Address)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Address)

data Link = Link
    { link_href :: Text.Text
    , link_type :: Text.Text
    } deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Link)

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
    toText a = Text.pack $ Aeson.camelTo2 '_' $ show a

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
    { newPayment_amount            :: Amount
    -- ^Set the amount to charge. Minimum based on available payment methods.
    , newPayment_description       :: Text.Text
    -- ^Set the description. Will be shown on card or bank statement.
    , newPayment_redirectUrl       :: Maybe Text.Text
    -- ^Set the url the customer will be redirected to after the payment process.
    , newPayment_webhookUrl        :: Maybe Text.Text
    -- ^Set a specific webhook for this payment.
    , newPayment_method            :: Maybe PaymentMethod
    -- ^Set a specific payment method for this payment. The customer will not have a choice when this is set.
    , newPayment_metadata          :: Maybe Aeson.Value
    -- ^Set any additional data in JSON format.
    , newPayment_locale            :: Maybe Text.Text
    -- ^Force the payment screen language.
    , newPayment_sequenceType      :: Maybe SequenceType
    -- ^Set the equence type, default to `Oneoff`. For more information see: https://www.mollie.com/en/docs/reference/customers/create-payment.
    , newPayment_customerId        :: Maybe Text.Text
    -- ^Set a customer account for this payment.
    , newPayment_mandateId         :: Maybe Int
    -- ^Set the ID of a specific Mandate. May be supplied to indicate which of the consumer’s accounts should be credited.
    -- IDEAL fields
    , newPayment_issuer            :: Maybe Text.Text
    -- CREDIT CARD
    , newPayment_billingAddress    :: Maybe Address
    -- ^Set card holder's address. This is to improve the credit card fraude protection.
    -- CREDIT CARD & PAYPAL
    , newPayment_shippingAddress   :: Maybe Address
    -- ^Set the shipping address. This is to improve fraude protection.
    -- BANK TRANSFER
    , newPayment_billingEmail      :: Maybe Text.Text
    -- ^Set the billing email address. Billing instructions will be send immediately when the payment is created. When no locale is set the email will be sent in English.
    , newPayment_dueDate           :: Maybe Text.Text
    -- ^Set the date this payment should expire, in `YYYY-MM-DD` format. Minimum date is tomorrow and maximum is 100 days from now.
    -- SEPA DIRECT DEBIT
    , newPayment_consumerName      :: Maybe Text.Text
    -- ^Set the beneficiary name of the account holder.
    , newPayment_consumerAccount   :: Maybe Text.Text
    -- ^Set the account holders IBAN.
    -- PAYSAFECARD
    , newPayment_customerReference :: Maybe Text.Text
    -- ^Set an identifier for the customer.
    }
    deriving (Show)

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
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
    { payment_id                :: Text.Text
    -- ^Mollie's reference to the payment resource.
    , payment_mode              :: Mode
    -- ^The mode used to create this payment.
    , payment_createdAt         :: Time.UTCTime
    -- ^The date on which the payment was created.
    , payment_status            :: PaymentStatus
    -- ^The current status.
    , payment_isCancelable      :: Bool
    -- ^Whether or not the payment can be canceled.
    , payment_paidAt            :: Maybe Time.UTCTime
    -- ^The date on which the payment was paid.
    , payment_canceledAt        :: Maybe Time.UTCTime
    -- ^The date on which the payment was canceled.
    , payment_expiredAt         :: Maybe Time.UTCTime
    -- ^The date on which the payment expired.
    , payment_failedAt          :: Maybe Time.UTCTime
    -- ^The date on which the payment failed.
    , payment_amount            :: Amount
    -- ^The amount charged for this payment.
    , payment_amountRefunded    :: Maybe Amount
    -- ^The amount which has been refunded.
    , payment_amountRemaining   :: Maybe Amount
    -- ^The amount which remained after refunding.
    , payment_description       :: Text.Text
    -- ^The payment description, as show on the bank or card statement.
    , payment_redirectUrl       :: Maybe Text.Text
    -- ^The URL your customer will be redirected to after completing or canceling the payment process
    , payment_webhookUrl        :: Maybe Text.Text
    -- ^The URL Mollie will call as soon an important status change takes place.
    , payment_method            :: Maybe PaymentMethod
    -- ^The payment method used.
    , payment_metadata          :: Maybe Aeson.Value
    -- ^Custom privided metadata.
    , payment_locale            :: Maybe Text.Text
    -- ^The language used during checkout.
    , payment_countryCode       :: Maybe Text.Text
    -- ^This optional field contains your customer’s ISO 3166-1 alpha-2 country code, detected during checkout.
    , payment_profileId         :: Text.Text
    -- ^Identifier for the profile this payment was created on.
    , payment_settlementAmount  :: Maybe Amount
    -- ^The amount that will be settled to your account.
    , payment_settlementId      :: Maybe Text.Text
    -- ^The identifier referring to the settlement this payment was settled with.
    , payment_customerId        :: Maybe Text.Text
    -- ^Identifier for the customer this payment was created for.
    , payment_sequenceType      :: Maybe SequenceType
    -- ^Indicates which type of payment this is in a recurring sequence. Set to oneoff by default.
    , payment_mandateId         :: Maybe Text.Text
    -- ^Identifier for the mandate used for this payment if it's recurring.
    , payment_subscriptionId    :: Maybe Text.Text
    -- ^Identifier for the subscription used for this payment.
    -- TODO: Add payment specific details, see: https://www.mollie.com/nl/docs/reference/payments/get
    , payment_details           :: Maybe Aeson.Object
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Payment)

{-|
  Important links associated with List responses.
-}
data ListLinks = ListLinks
    { listLinks_self          :: Link
    -- ^The URL to the current set of objects..
    , listLinks_next          :: Maybe Link
    -- ^The previous set of objects, if available.
    , listLinks_previous      :: Maybe Link
    -- ^The next set of objects, if available.
    , listLinks_documentation :: Maybe Link
    -- ^URL to the documentation of the current endpoint.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''ListLinks)

{-|
  List response for any resource with metadata.

  For more information see: https://www.mollie.com/nl/docs/reference/payments/list.
-}
data List a = List
    { list_count     :: Int
    -- ^The number of objects found in `_embedded`, which is either the requested number (with a maximum of 250) or the default number.
    , list__embedded :: [a]
    -- ^The actual data you’re looking for.
    , list__links    :: ListLinks
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

{-|
  Structure to request a refund.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/create.
-}
data NewRefund = NewRefund
    { newRefund_amount      :: Maybe Amount
    -- ^The amount to refund. For some payments, it can be up to €25.00 more than the original transaction amount.
    , newRefund_description :: Maybe Text.Text
    -- ^Set the description. Will be shown on card or bank statement.
    }
    deriving (Show)

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
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
    { refund_id               :: Text.Text
    -- ^Mollies reference to the refund.
    , refund_amount           :: Amount
    -- ^The amount refunded to your customer with this refund.
    , refund_settlementAmount :: Maybe Amount
    -- ^This optional field will contain the amount that will be deducted from your account balance.
    , refund_description      :: Text.Text
    -- ^The description of the refund that may be shown to your customer.
    , refund_status           :: RefundStatus
    -- ^The status in which this refund currently is.
    , refund_paymentId        :: Text.Text
    -- ^The unique identifier of the payment this refund was created for.
    , refund_payment          :: Maybe Payment
    -- ^The payment this refund was made for.
    , refund_createdAt        :: Time.UTCTime
    -- ^The date and time the refund was issued.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Refund)

{-|
  Images associated with a payment method.
-}
data MethodImage = MethodImage
    { methodImage_size1x :: Text.Text
    -- ^Normal method icon, 32x24 pixels.
    , methodImage_size2x :: Text.Text
    -- ^Bigger method icon, 64x48px pixels.
    , methodImage_svg    :: Text.Text
    -- ^Vector icon, can scale to any size.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''MethodImage)

{-|
  Representation of a payment method available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/methods/get.
-}
data Method = Method
    { method_id          :: PaymentMethod
    -- ^Mollies reference to the method.
    , method_description :: Text.Text
    -- ^Full name of the method. This value changes based on requested locale.
    , method_image       :: MethodImage
    -- ^Icons for this method.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Method)

{-|
  Representation of an issuer available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/issuers/get.
-}
data Issuer = Issuer
    { issuer_id     :: Text.Text
    -- ^Mollies reference to the issuer.
    , issuer_name   :: Text.Text
    -- ^The issuers full name.
    , issuer_method :: PaymentMethod
    -- ^The payment method this issuer belongs to. Currently only Ideal is supported.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Issuer)

{-|
  Structure to request a new customer with.

  For more information see: https://www.mollie.com/en/docs/reference/customers/create.
-}
data NewCustomer = NewCustomer
    { newCustomer_name     :: Maybe Text.Text
    -- ^Set the full name of the customer.
    , newCustomer_email    :: Maybe Text.Text
    -- ^Set the email address.
    , newCustomer_locale   :: Maybe Text.Text
    -- ^Set the language to use for this customer during checkout,
    , newCustomer_metadata :: Maybe Aeson.Value
    -- ^Set any additional data in JSON format.
    }
    deriving (Show)

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''NewCustomer)

{-|
  Representation of an customer available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/customers/get.
-}
data Customer = Customer
    { customer_id                  :: Text.Text
    -- ^Mollies reference to the customer.
    , customer_mode                :: Mode
    -- ^The mode in which this customer was created.
    , customer_name                :: Maybe Text.Text
    -- ^The customers full name.
    , customer_email               :: Maybe Text.Text
    -- ^The cusomters email address.
    , customer_locale              :: Maybe Text.Text
    -- ^The locale used for this customer during checkout.
    , customer_metadata            :: Maybe Aeson.Value
    -- ^Custom privided data for this customer.
    , customer_recentlyUsedMethods :: [PaymentMethod]
    -- ^The payment methods this customer recently used.
    , customer_createdAt     :: Time.UTCTime
    -- ^The creation date of this customer.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Customer)

{-|
  Structure to request a new mandate with.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/create.
-}
data NewMandate = NewMandate
    { newMandate_method           :: PaymentMethod
    -- ^Set the payment method of the mandate. Currently only directdebit is supported.
    , newMandate_consumerName     :: Text.Text
    -- ^Set the consumer's name.
    , newMandate_consumerAccount  :: Text.Text
    -- ^Set the consumer's IBAN.
    , newMandate_consumerBic      :: Maybe Text.Text
    -- ^Set the consumer's bank BIC/SWIFT code.
    , newMandate_signatureDate    :: Maybe Text.Text
    -- ^Set the date the mandate was signed in `YYYY-MM-DD` format.
    , newMandate_mandateReference :: Maybe Text.Text
    -- ^Set a custom reference to this mandate.
    }
    deriving (Show)

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
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
    { mandateDetails_consumerName    :: Maybe Text.Text
    -- ^The direct debit account holder's name.
    , mandateDetails_consumerAccount :: Maybe Text.Text
    -- ^The direct debit account IBAN.
    , mandateDetails_consumerBic     :: Maybe Text.Text
    -- ^The direct debit account BIC.
    , mandateDetails_cardHolder      :: Maybe Text.Text
    -- ^The credit card holder's name.
    , mandateDetails_cardNumber      :: Maybe Text.Text
    -- ^The last 4 digits of the credit card number.
    , mandateDetails_cardLabel       :: Maybe Text.Text
    -- ^The credit card's label.
    , mandateDetails_cardFingerprint :: Maybe Text.Text
    -- ^Unique alphanumeric representation of a credit card. Usable to identify returning customers.
    , mandateDetails_cardExpiryDate  :: Maybe Text.Text
    -- ^The credit card's expiry date in `YYYY-MM-DD` format.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''MandateDetails)

{-|
  Representation of a mandate available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/get.
-}
data Mandate = Mandate
    { mandate_id               :: Text.Text
    -- ^Mollies reference to the mandate.
    , mandate_status           :: MandateStatus
    -- ^The status of the mandate.
    , mandate_method           :: PaymentMethod
    -- ^The payment method of the mandate.
    , mandate_details          :: Maybe MandateDetails
    -- ^The mandate details.
    , mandate_mandateReference :: Maybe Text.Text
    -- ^The custom reference set for this mandate.
    , mandate_signatureDate    :: Maybe Text.Text
    -- ^Set the date the mandate was signed in `YYYY-MM-DD` format.
    , mandate_createdAt        :: Time.UTCTime
    -- ^The date on which this mandate was created.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Mandate)

{-|
  Structure to request a new subscription with.

  For more information see: https://www.mollie.com/en/docs/reference/subscriptions/create.
-}
data NewSubscription = NewSubscription
    { newSubscription_amount      :: Amount
    -- ^Set the amount you want to charge each subscription cycle.
    , newSubscription_times       :: Maybe Int
    -- ^Set the total number of charges for the subscription to complete. Leave empty for ongoing subscriptions.
    , newSubscription_interval    :: Text.Text
    -- ^Set the interval to wait between charges like `1 month(s)`, `2 weeks` or `14 days`.
    , newSubscription_startDate   :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format. This is the first day on which your customer will be charged. When this parameter is not provided, the current date will be used instead.
    , newSubscription_description :: Text.Text
    -- ^Set the description which will be included in the payment description along with the carge date in `Y-m-d` format.
    , newSubscription_method      :: Maybe PaymentMethod
    -- ^Force the payment method, leave empty to use one of the customers valid mandates.
    , newSubscription_webhookUrl  :: Maybe Text.Text
    -- ^Set a webhook URL for all subscription payments.
    }
    deriving (Show)

$(Aeson.deriveToJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
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
    { subscription_id                :: Text.Text
    -- ^Mollies reference to the subscription.
    , subscription_mode              :: Mode
    -- ^The mode used to create this subscription
    , subscription_createdAt         :: Time.UTCTime
    -- ^The date on which this subscription was created.
    , subscription_status            :: SubscriptionStatus
    -- ^The subscriptions status.
    , subscription_amount            :: Amount
    -- ^The amount charged with each payment for this subscription.
    , subscription_times             :: Maybe Int
    -- ^The total number or charges for the subscription to complete.
    , subscription_interval          :: Text.Text
    -- ^The interval to wait between charges.
    , subscription_startDate         :: Maybe Text.Text
    -- ^Set the start date of the subscription in YYYY-MM-DD format.
    , subscription_description       :: Text.Text
    -- ^The description for the payments made with this subscription.
    , subscription_method            :: Maybe PaymentMethod
    -- ^The payment method used for this subscription.
    , subscription_canceledAt        :: Maybe Time.UTCTime
    -- ^The date on which this subscription was canceled.
    , subscription_webhookUrl        :: Maybe Text.Text
    -- ^The URL Mollie will call as soon a payment status change takes place.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Subscription)

data Chargeback = Chargeback
    { chargeback_id               :: Text.Text
    -- ^Mollies reference to the chargeback.
    , chargeback_amount           :: Amount
    -- ^The amount charged back by the consumer.
    , chargeback_settlementAmount :: Amount
    -- ^The amount that will be deducted from your account
    , chargeback_createdAt        :: Time.UTCTime
    -- ^The date and time the chargeback was issued.
    , chargeback_reversedAt       :: Maybe Time.UTCTime
    -- ^The date and time the chargeback was issued.
    , chargeback_paymentId        :: Text.Text
    -- ^The unique identifier of the payment this chargeback was issued for.
    , chargeback_payment          :: Maybe Payment
    -- ^The payment this chargeback was made for.
    }

{-|
  Error data representations.

  For more information see: https://www.mollie.com/en/docs/errors.
-}
data ErrorLinks = ErrorLinks
    { errorLinks_documentation :: Link
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''ErrorLinks)

data Error = Error
    { error_title   :: Text.Text
    , error_detail  :: Text.Text
    , error_field   :: Maybe Text.Text
    , error__links  :: Maybe ErrorLinks
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Error)


{-|
  Response errors which could happen when requesting resources from Mollie.
-}
data ResponseError
    = ClientError Int Error
    | ServerError Int
    | UnexpectedResponse Text.Text
    deriving (Show)
