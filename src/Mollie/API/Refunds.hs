{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Refunds
    ( refundsPath
    , getRefunds
    -- Re-export relevant types
    , RefundStatus (..)
    , Refund (..)
    -- Lens getters
    , Mollie.API.Refunds.id
    , amount
    , description
    , settlementAmount
    , status
    , paymentId
    , payment
    , createdAt
    ) where

import           Control.Lens        (makeFieldsNoPrefix)
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Types

makeFieldsNoPrefix ''NewRefund
makeFieldsNoPrefix ''Refund

{-|
  Refund resource's path, relative to API's versioned url or to a Payment resource url.
-}
refundsPath :: Text.Text
refundsPath = "refunds"

{-|
  Handler to get a list of refunds. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of refunds returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/list-all.
-}
getRefunds :: Int -- ^ offset
           -> Int -- ^ count
           -> Mollie (Either ResponseError (List Refund))
getRefunds offset count = get path
    where
        path = refundsPath <> query
        query = "?offset=" <> showT offset <> "&count=" <> showT count
