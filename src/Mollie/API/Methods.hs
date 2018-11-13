{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Methods
    ( getMethod
    , getMethods
    , PaymentMethod (..)
    , MethodImage (..)
    , Method (..)
    , MethodAPI (..)
    -- Lens getters
    , Mollie.API.Methods.id
    , description
    , image
    , size1x
    , size2x
    , svg
    ) where

import qualified Control.Lens        as Lens
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Aeson.Types    as Aeson
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

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

instance ToHttpApiData PaymentMethod where
    toUrlPiece a = toText a

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

Lens.makeFieldsNoPrefix ''MethodImage

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

Lens.makeFieldsNoPrefix ''Method

data MethodAPI route = MethodAPI
    { getMethods :: route :- "methods"
                    :> QueryParam "limit" Int
                    :> Get '[HalJSON] (List Method)
    , getMethod  :: route :- "methods"
                    :> Capture "id" PaymentMethod
                    :> Get '[HalJSON] Method
    } deriving Generic
