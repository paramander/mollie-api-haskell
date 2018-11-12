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
import           Data.Monoid         ((<>))
import qualified Data.Text           as Text
import qualified Data.Time           as Time
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

{-|
  For usage in API calls.
-}
data QueryParam = QueryParam Text.Text Text.Text

queryParam :: Text.Text -> Text.Text -> QueryParam
queryParam = QueryParam

instance ToText [QueryParam] where
    toText [] = mempty
    toText params =
        mappend "?" $ Text.intercalate "&" $ map (\(QueryParam paramName paramValue) -> paramName <> "=" <> paramValue) params
