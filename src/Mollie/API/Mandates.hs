{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

{-|
  [WARNING]: This implementation is currently untested! Due to lack of access to the Mandates API.
-}
module Mollie.API.Mandates
    ( MandateAPI
    , newMandate
    , createCustomerMandate
    , getCustomerMandate
    , getCustomerMandates
    , NewMandate (..)
    , MandateStatus (..)
    , MandateDetails (..)
    , MandateId
    , Mandate (..)
    -- Lens getters
    , Mollie.API.Mandates.id
    , status
    , method
    , details
    , mandateReference
    , signatureDate
    , createdAt
    , consumerName
    , consumerAccount
    , consumerBic
    , cardHolder
    , cardNumber
    , cardLabel
    , cardFingerprint
    , cardExpiryDate
    ) where

import           Control.Lens         (makeFieldsNoPrefix, (&), (.~))
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import           Data.Default         (Default, def)
import qualified Data.Text            as Text
import qualified Data.Time            as Time
import           GHC.Generics         (Generic)
import qualified Mollie.API.Customers as Customers
import           Mollie.API.Internal  (HalJSON)
import           Mollie.API.Methods   (PaymentMethod (..))
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

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

makeFieldsNoPrefix ''NewMandate

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

makeFieldsNoPrefix ''MandateDetails

{-|
  Representation of a mandate available at Mollie.

  For more information see: https://www.mollie.com/en/docs/reference/mandates/get.
-}
data Mandate = Mandate
    { _id               :: MandateId
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

makeFieldsNoPrefix ''Mandate

newMandate :: PaymentMethod
           -> Text.Text -- ^ consumerName
           -> Text.Text -- ^ consumerAccount
           -> NewMandate
newMandate _method _consumerName _consumerAccount =
    def
      & method .~ _method
      & consumerName .~ _consumerName
      & consumerAccount .~ _consumerAccount

data MandateAPI route = MandateAPI
    { getCustomerMandates   :: route :- "customers" :> Capture "customerId" CustomerId :> "mandates" :> Get '[HalJSON] (List Mandate)
    , createCustomerMandate :: route :- "customers" :> Capture "customerId" CustomerId :> "mandates" :> ReqBody '[JSON] NewMandate :> Post '[HalJSON] Mandate
    , getCustomerMandate    :: route :- "customers" :> Capture "customerId" CustomerId :> "mandates" :> Capture "id" MandateId :> Get '[HalJSON] Mandate
    } deriving Generic
