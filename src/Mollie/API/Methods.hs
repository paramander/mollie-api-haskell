{-# LANGUAGE OverloadedStrings #-}

module Mollie.API.Methods
    ( methodsPath
    , getMethod
    , getMethods
    -- Re-export relevant types
    , PaymentMethod (..)
    , MethodImage (..)
    , Method (..)
    , ListLinks (..)
    , List (..)
    , ResponseError (..)
    ) where

import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Types

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
          -> Text.Text -- ^ locale
          -> Mollie (Either ResponseError Method)
getMethod methodId locale = get path
    where
        path = Text.intercalate "/" [methodsPath, toText methodId] <> query
        query = "?locale=" <> locale

{-|
  Handler to get a list of payment methods. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of payment methods returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/methods/list.
-}
getMethods :: Text.Text -- ^ locale
           -> Int -- ^ offset
           -> Int -- ^ count
           -> Mollie (Either ResponseError (List Method))
getMethods locale offset count = get path
    where
        path = methodsPath <> query
        query = "?locale=" <> locale <> "&offset=" <> showT offset <> "&count=" <> showT count
