{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Types where

import qualified Control.Lens        as Lens
import           Control.Lens.TH     ()
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Aeson.Types    as Aeson
import           Data.Char           (toLower)
import qualified Data.Currency       as Currency
import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
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
  Response errors which could happen when requesting resources from Mollie.
-}
data ResponseError
    = ClientError Int Error
    | ServerError Int
    | UnexpectedResponse Text.Text
    deriving (Show)
