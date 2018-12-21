{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Mandates
    ( MandateAPI
    , newMandate
    , createCustomerMandate
    , getCustomerMandate
    , getCustomerMandates
    , getCustomerMandatesPaginated
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

import           Control.Lens         (makeFields, (&), (.~))
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import           Data.Default         (Default, def)
import qualified Data.Text            as Text
import qualified Data.Time            as Time
import           GHC.Generics         (Generic)
import qualified Mollie.API.Customers as Customers
import           Mollie.API.Helpers
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

makeFields ''NewMandate

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

makeFields ''MandateDetails

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

makeFields ''Mandate

newMandate :: PaymentMethod -- ^ _method
           -> Text.Text -- ^ _consumerName
           -> Text.Text -- ^ _consumerAccount
           -> NewMandate
newMandate _method _consumerName _consumerAccount =
    def
      & method .~ _method
      & consumerName .~ _consumerName
      & consumerAccount .~ _consumerAccount

data MandateAPI route = MandateAPI
    { getCustomerMandatesPaginated :: route :- "customers"
                                      :> Capture "customerId" Customers.CustomerId
                                      :> "mandates"
                                      :> QueryParam "limit" Int
                                      :> QueryParam "from" MandateId
                                      :> Get '[HalJSON] (List Mandate)
    -- ^Handler to get a paginated list of mandates for a specific customer. Offset the results by passing the last mandate ID in the `from` query param. The mandate with this ID is included in the result set as well. See https://docs.mollie.com/reference/v2/mandates-api/list-mandates
    --
    -- Example for fetching the last mandate for a customer:
    --
    -- @
    -- import Mollie.API
    -- import Mollie.API.Mandates
    --
    -- env <- createEnv "test_mollieapikeyexample"
    -- let customerMandatesResult = runMollie env (getCustomerMandatesPaginated customerClient "cst_exampleid" (Just 1) Nothing)
    -- @
    , getCustomerMandates          :: route :- "customers"
                                      :> Capture "customerId" Customers.CustomerId
                                      :> "mandates"
                                      :> Get '[HalJSON] (List Mandate)
    -- ^Handler to get a paginated list of mandates for a specific customer. Applies default pagination for newest 250 customers. See https://docs.mollie.com/reference/v2/mandates-api/list-mandates
    , createCustomerMandate        :: route :- "customers"
                                      :> Capture "customerId" Customers.CustomerId
                                      :> "mandates"
                                      :> ReqBody '[JSON] NewMandate
                                      :> Post '[HalJSON] Mandate
    -- ^Handler to create a new mandate for a specific customer. See https://docs.mollie.com/reference/v2/mandates-api/create-mandate
    , getCustomerMandate           :: route :- "customers"
                                      :> Capture "customerId" CustomerId
                                      :> "mandates"
                                      :> Capture "id" MandateId
                                      :> Get '[HalJSON] Mandate
    -- ^Handler to get a mandate by its identifier for a specific customer. See https://docs.mollie.com/reference/v2/mandates-api/get-mandate
    } deriving Generic
