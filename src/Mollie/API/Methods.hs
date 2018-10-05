{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Methods
    ( methodsPath
    , getMethod
    , getMethods
    , PaymentMethod (..)
    , MethodImage (..)
    , Method (..)
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
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Types

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

{-|
  Payment method resource's path, relative to API's versioned url.
-}
methodsPath :: Text.Text
methodsPath = "methods"

{-|
  Handler to get a payment method by its identifier.

  Fails on payment methods which are not enabled.

  For more information see: https://www.mollie.com/en/docs/reference/methods/get.
-}
getMethod :: PaymentMethod
          -> [QueryParam] -- ^ queryParams
          -> Mollie (Either ResponseError Method)
getMethod methodId queryParams = get path
    where
        path = Text.intercalate "/" [methodsPath, toText methodId] <> toText queryParams

{-|
  Handler to get a list of payment methods. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payment methods returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/methods/list.
-}
getMethods :: [QueryParam] -- ^ queryParams
           -> Mollie (Either ResponseError (List Method))
getMethods queryParams = get path
    where
        path = methodsPath <> toText queryParams
