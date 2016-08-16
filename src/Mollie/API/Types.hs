{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mollie.API.Types where

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text            as Text
import qualified Data.Time            as Time

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
    | PaymentCancelled
    -- ^Customer has cancelled the payment.
    | PaymentPending
    -- ^The payment process has been started. No notification.
    | PaymentExpired
    -- ^The payment has expired. Some payment methods (like `banktransfer`) might need a few days to process.
    | PaymentFailed
    -- ^The payment can't be completed.
    | PaymentPaid
    -- ^The payment was successful. This is the success status.
    | PaymentPaidout
    -- ^Mollie has transfered the payment to your bankaccount. No notification.
    | PaymentRefunded
    -- ^You requested a refund for the payment.
    | PaymentChargedBack
    -- ^The customer dispute the payment. This is possible with `creditcard`, `directdebit` and `paypal` payments.
    deriving (Show, Eq)

$(Aeson.deriveJSON
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
    | Mistercard
    | Sofort
    | Banktransfer
    | Directdebit
    | Belfius
    | Paypal
    | Bitcoin
    | Podiumcadeaukaart
    | Paysafecard
    | NewPaymentMethod Text.Text -- When this shows up in a response from or is required for a request to Mollie contact package maintainer.
    deriving (Eq)

instance Show PaymentMethod where
    show Ideal                   = "ideal"
    show Creditcard              = "creditcard"
    show Mistercard              = "mistercard"
    show Sofort                  = "sofort"
    show Banktransfer            = "banktransfer"
    show Directdebit             = "directdebit"
    show Belfius                 = "belfius"
    show Paypal                  = "paypal"
    show Bitcoin                 = "bitcoin"
    show Podiumcadeaukaart       = "podiumcadeaukaart"
    show Paysafecard             = "paysafecard"
    show (NewPaymentMethod text) = Text.unpack text

instance Aeson.ToJSON PaymentMethod where
    toJSON method = Aeson.String . Text.pack $ show method

instance Aeson.FromJSON PaymentMethod where
    parseJSON val = case lookup val methods of
        Just method -> return method
        Nothing -> case val of
            (Aeson.String method) -> return $ NewPaymentMethod method
            invalid -> Aeson.typeMismatch "PaymentMethod" invalid
        where methods = map
                  (\method -> (Aeson.toJSON method, method))
                  [ Ideal, Creditcard, Mistercard, Sofort, Banktransfer
                  , Directdebit, Belfius, Paypal, Bitcoin, Podiumcadeaukaart
                  , Paysafecard
                  ]

{-|
  All available languages for Mollie's payment screen.
-}
data Locale
    = De
    | En
    | Es
    | Fr
    | Be
    | BeFr
    | Nl
    | NewLocale Text.Text -- When this shows up in a response from or is required for a request to Mollie contact package maintainer.
    deriving (Show, Eq)

instance Aeson.ToJSON Locale where
    toJSON (NewLocale lang) = Aeson.String lang
    toJSON lang = Aeson.String . Text.pack . Aeson.camelTo2 '-' $ show lang

instance Aeson.FromJSON Locale where
    parseJSON val = case lookup val locales of
        Just lang -> return lang
        Nothing -> case val of
            (Aeson.String lang) -> return $ NewLocale lang
            invalid -> Aeson.typeMismatch "Locale" invalid
        where locales = map
                  (\loc -> (Aeson.toJSON loc, loc))
                  [ De, En, Es, Fr, Be, BeFr, Nl
                  ]

{-|
  All available recurring types.
-}
data RecurringType
    = First
    | Recurring
    deriving (Show, Eq)

$(Aeson.deriveJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_'
        }
    ''RecurringType)

{-|
  Structure to request a new payment with.

  For more information see: https://www.mollie.com/en/docs/reference/payments/create.
-}
data NewPayment = NewPayment
    { newPayment_amount            :: Double
    -- ^Set the amount in EURO to charge. Minimum based on available payment methods.
    , newPayment_description       :: Text.Text
    -- ^Set the description. Will be shown on card or bank statement.
    , newPayment_redirectUrl       :: Text.Text
    -- ^Set the url the customer will be redirected to after the payment process.
    , newPayment_webhookUrl        :: Maybe Text.Text
    -- ^Set a specific webhook for this payment.
    , newPayment_method            :: Maybe PaymentMethod
    -- ^Set a specific payment method for this payment. The customer will not have a choice when this is set.
    , newPayment_metadata          :: Maybe Aeson.Value
    -- ^Set any additional data in JSON format.
    , newPayment_locale            :: Maybe Locale
    -- ^Force the payment screen language.
    , newPayment_recurringType     :: Maybe RecurringType
    -- ^Set the recurring type, for more information see: https://www.mollie.com/en/docs/reference/customers/create-payment.
    , newPayment_customerId        :: Maybe Text.Text
    -- ^Set a customer account for this payment.
    -- IDEAL fields
    , newPayment_issuer            :: Maybe Text.Text
    -- CREDIT CARD
    , newPayment_billingAddress    :: Maybe Text.Text
    -- ^Set card holder's address. This is to improve the credit card fraude protection.
    , newPayment_billingCity       :: Maybe Text.Text
    -- ^Set the card holder's city.
    , newPayment_billingRegion     :: Maybe Text.Text
    -- ^Set the card holder's region.
    , newPayment_billingPostal     :: Maybe Text.Text
    -- ^Set the card holder's postal code.
    , newPayment_billingCountry    :: Maybe Text.Text
    -- ^Set the card holder's country in https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 format.
    -- CREDIT CARD & PAYPAL
    , newPayment_shippingAddress   :: Maybe Text.Text
    -- ^Set the shipping address. This is to improve fraude protection. When used with PayPal the maximum length is 128 characters.
    , newPayment_shippingCity      :: Maybe Text.Text
    -- ^Set the shipping city. When used with PayPal the maximum length is 100 characters.
    , newPayment_shippingRegion    :: Maybe Text.Text
    -- ^Set the shipping region. When used with PayPal the maximum length is 100 characters and this field is required when the shipping country is one of the following countries: `AR` `BR` `CA` `CN` `ID` `IN` `JP` `MX` `TH` `US`.
    , newPayment_shippingPostal    :: Maybe Text.Text
    -- ^Set the shipping postal code. When used with PayPal the maximum length is 20 characters.
    , newPayment_shippingCountry   :: Maybe Text.Text
    -- ^Set the shipping country in https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 format.
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
  All available payment modes.
-}
data PaymentMode
    = Live
    | Test
    deriving (Show, Eq)

$(Aeson.deriveJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_'
        }
    ''PaymentMode)

{-|
  Important links used for a payment.
-}
data PaymentLinks = PaymentLinks
    { paymentLinks_paymentUrl  :: Maybe Text.Text
    -- ^URL where the customer should be redirected to pay.
    , paymentLinks_webhookUrl  :: Maybe Text.Text
    -- ^URL for the webhook called on status changes.
    , paymentLinks_redirectUrl :: Text.Text
    -- ^URL where the customer will be redirected to by Mollie after checkout.
    , paymentLinks_settlement  :: Maybe Text.Text
    -- ^URL to the settlement resource this payment belongs to.
    , paymentLinks_refunds     :: Maybe Text.Text
    -- ^URL to the refund resources for this payment.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''PaymentLinks)

{-|
  Representation of a payment made with Mollie.

  Note that the amounts are curently returned as text because Mollie does not return them as valid json numbers.

  For more information see: https://www.mollie.com/nl/docs/reference/payments/get.
-}
data Payment = Payment
    { payment_id                :: Text.Text
    -- ^Mollie's reference to the payment resource.
    , payment_mode              :: PaymentMode
    -- ^The mode used to create this payment.
    , payment_createdDatetime   :: Time.UTCTime
    -- ^The date on which the payment was created.
    , payment_status            :: PaymentStatus
    -- ^The current status.
    , payment_paidDatetime      :: Maybe Time.UTCTime
    -- ^The date on which the payment was paid.
    , payment_cancelledDatetime :: Maybe Time.UTCTime
    -- ^The date on which the payment was cancelled.
    , payment_expiredDatetime   :: Maybe Time.UTCTime
    -- ^The date on which the payment expired.
    , payment_expiryPeriod      :: Maybe Text.Text
    -- ^The expiry period assigned to the payment, format: https://en.wikipedia.org/wiki/ISO_8601#Durations.
    , payment_amount            :: Text.Text -- FAIL: Amount is currently being returned as String
    -- ^The amount of EURO charged for this payment.
    , payment_amountRefunded    :: Maybe Text.Text -- FAIL: Amount is currently being returned as String
    -- ^The amount of EURO which has been refunded.
    , payment_amountRemaining   :: Maybe Text.Text -- FAIL: Amount is currently being returned as String
    -- ^The amount of EURO which remained after refunding.
    , payment_description       :: Text.Text
    -- ^The payment description, as show on the bank or card statement.
    , payment_method            :: Maybe PaymentMethod
    -- ^The payment method used.
    , payment_metadata          :: Maybe Aeson.Value
    -- ^Custom privided metadata.
    , payment_locale            :: Maybe Locale
    -- ^The language used during checkout.
    , payment_profileId         :: Text.Text
    -- ^Identifier for the profile this payment was created on.
    , payment_settlementId      :: Maybe Text.Text
    -- ^Identifier for the settlement this payment belongs to.
    , payment_links             :: PaymentLinks
    -- ^Important links used for this payment.
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
    { listLinks_previous :: Maybe Text.Text
    -- ^URL to previous list.
    , listLinks_next     :: Maybe Text.Text
    -- ^URL to next list.
    , listLinks_first    :: Maybe Text.Text
    -- ^URL to first list.
    , listLinks_last     :: Maybe Text.Text
    -- ^URL to last list.
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
    { list_totalCount :: Int
    -- ^Total number of resources available.
    , list_offset     :: Int
    -- ^The number of skipped resources.
    , list_count      :: Int
    -- ^The number of resources found in `data`.
    , list_data       :: [a]
    -- ^The resources for this request.
    , list_links      :: Maybe ListLinks
    -- ^Important links for this list.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''List)

{-|
  Structure to request a refund.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/create.
-}
data NewRefund = NewRefund
    { newRefund_amount      :: Maybe Double
    -- ^Set the amount in EURO that should be refunded. If left `Nothing` the full amount of the targetted payment will be refunded.
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
    = RefundPending
    -- ^The payment will be processed soon (usually the next business day). The refund could still be cancelled, see: https://www.mollie.com/en/docs/reference/refunds/delete.
    | RefundProcessing
    -- ^The refund is processing, cancellation is no longer possible.
    | RefundRefunded
    -- ^The refund has been paid out the the customer.
    deriving (Show, Eq)

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
    , refund_payment          :: Payment
    -- ^The payment for which this refund was made.
    , refund_amount           :: Text.Text -- FAIL: Amount is currently being returned as String
    -- ^The amount of EURO which this refund refunded.
    , refund_status           :: RefundStatus
    -- ^The status in which this refund currently is.
    , refund_refundedDatetime :: Maybe Time.UTCTime
    -- ^The date on which the refund was issued.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''Refund)

{-|
  Minimum and maximum amounts for a payment method.

  Note that the amounts are curently returned as text because Mollie does not return them as valid json numbers.
-}
data MethodAmount = MethodAmount
    { methodAmount_minimum :: Text.Text -- FAIL: Amount is currently being returned as String
    -- ^The minimum amount of EURO for which it's possible to use this method.
    , methodAmount_maximum :: Text.Text -- FAIL: Amount is currently being returned as String
    -- ^The maximum amount of EURO for which it's possible to use this method.
    }
    deriving (Show)

$(Aeson.deriveFromJSON
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1 . snd . break (== '_')
        }
    ''MethodAmount)

{-|
  Images associated with a payment method.
-}
data MethodImage = MethodImage
    { methodImage_normal :: Text.Text
    -- ^Normal method icon, 40x40 pixels.
    , methodImage_bigger :: Text.Text
    -- ^Bigger method icon, 80x80px pixels.
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
    , method_amount      :: MethodAmount
    -- ^Range for this method in EURO.
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
  Failures which could happen when requesting resources from Mollie.
-}
data Failure
    = RequestFailure Int ByteString.ByteString -- TODO: Add more specific request errors
    | ParseFailure ByteString.ByteString
    | NotFound
    deriving (Show, Eq)
