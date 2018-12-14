{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
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
    , MethodAPI
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
import           Mollie.API.Helpers
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

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
                  [ Bancontact, Banktransfer, Belfius, Bitcoin
                  , Creditcard, Directdebit, Eps, Giftcard
                  , Giropay, Ideal, Inghomepay, Kbc
                  , Paypal, Paysafecard, Sofort
                  ]

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

data MethodAPI route = MethodAPI
    { getMethods :: route :- "methods"
                    :> Get '[HalJSON] (List Method)
    -- ^Handler to get a list of payment methods. See https://docs.mollie.com/reference/v2/methods-api/list-methods
    , getMethod  :: route :- "methods"
                    :> Capture "id" PaymentMethod
                    :> Get '[HalJSON] Method
    -- ^Handler to get a payment method by its identifier. See https://docs.mollie.com/reference/v2/methods-api/get-method
    } deriving Generic
