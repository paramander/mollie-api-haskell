{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Issuers
    ( issuersPath
    , getIssuer
    , getIssuers
    -- Re-export relevant types
    , Issuer (..)
    -- Lens getters
    , Mollie.API.Issuers.id
    , name
    , method
    ) where

import qualified Control.Lens        as Lens
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Types

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
getIssuers :: Int -- ^ offset
           -> Int -- ^ count
           -> Mollie (Either ResponseError (List Issuer))
getIssuers offset count = get path
    where
        path = issuersPath <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
