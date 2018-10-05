{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Issuers
    ( issuersPath
    , getIssuer
    , getIssuers
    , Issuer (..)
    -- Lens getters
    , Mollie.API.Issuers.id
    , name
    , method
    ) where

import qualified Control.Lens        as Lens
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Methods  (PaymentMethod (..))
import           Mollie.API.Types

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

Lens.makeFieldsNoPrefix ''Issuer

{-|
  Issuer resource's path, relative to API's versioned url.
-}
issuersPath :: Text.Text
issuersPath = "issuers"

{-|
  Handler to get an issuer by its identifier.

  For more information see: https://www.mollie.com/en/docs/reference/issuers/get.
-}
getIssuer :: Text.Text -- ^ issuerId
          -> Mollie (Either ResponseError Issuer)
getIssuer issuerId = get path
    where
        path = Text.intercalate "/" [issuersPath, issuerId]

{-|
  Handler to get a list of issuers. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payment methods returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/issuers/list.
-}
getIssuers :: [QueryParam] -- ^ queryParams
           -> Mollie (Either ResponseError (List Issuer))
getIssuers queryParams = get path
    where
        path = issuersPath <> toText queryParams
