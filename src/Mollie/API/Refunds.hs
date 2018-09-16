{-# LANGUAGE OverloadedStrings #-}

module Mollie.API.Refunds
    ( refundsPath
    , getRefunds
    -- Re-export relevant types
    , Amount (..)
    , RefundStatus (..)
    , Refund (..)
    , ListLinks (..)
    , List (..)
    , ResponseError (..)
    ) where

import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Types

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
