{-# LANGUAGE OverloadedStrings #-}

module Mollie.API.Refunds
    ( refundsPath
    , newRefund
    , createPaymentRefund
    , getPaymentRefund
    , cancelPaymentRefund
    , getPaymentRefunds
    , getRefunds
    -- Re-export relevant types
    , NewRefund (..)
    , RefundStatus (..)
    , Refund (..)
    , ListLinks (..)
    , List (..)
    , Failure (..)
    ) where

import qualified Data.Aeson          as Aeson
import           Data.Monoid
import qualified Data.Text           as Text
import           Mollie.API.Internal
import           Mollie.API.Payments
import           Mollie.API.Types
import qualified Network.HTTP.Types  as HTTP

{-|
  Refund resource's path, relative to API's versioned url or to a Payment resource url.
-}
refundsPath :: Text.Text
refundsPath = "refunds"

{-|
  Helper to create a minimal new refund. Defaults to refunding the total amount for the targetted payment.
-}
newRefund :: NewRefund
newRefund = NewRefund
    { newRefund_amount      = Nothing
    , newRefund_description = Nothing
    }

{-|
  Handler to create a new refund for a specific payment.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/create.
-}
createPaymentRefund :: Text.Text -> NewRefund -> Mollie (Either Failure Refund)
createPaymentRefund paymentId newRefund = do
    (statusCode, rawBody) <- send HTTP.methodPost path newRefund
    return $ case statusCode of
        201 -> case Aeson.decode rawBody of
            Just refund -> Right refund
            Nothing     -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath]

{-|
  Handler to get a refund by its identifier for a specific payment.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/get.
-}
getPaymentRefund :: Text.Text -> Text.Text -> Mollie (Either Failure Refund)
getPaymentRefund paymentId refundId = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just refund -> Right refund
            Nothing      -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath, refundId]

{-|
  Handler to cancel a refund by its identifier for a specific payment.

  This request only works on refunds which have not yet started processing.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/delete.
-}
cancelPaymentRefund :: Text.Text -> Text.Text -> Mollie (Maybe Failure)
cancelPaymentRefund paymentId refundId = do
    (statusCode, rawBody) <- delete path
    return $ case statusCode of
        204 -> Nothing
        404 -> Just NotFound
        _   -> Just $ RequestFailure statusCode rawBody
    where
        path = Text.intercalate "/" [paymentsPath, paymentId, refundsPath, refundId]

{-|
  Handler to get a list of refunds for a specific payment. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of refunds returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/list.
-}
getPaymentRefunds :: Text.Text -> Int -> Int -> Mollie (Either Failure (List Refund))
getPaymentRefunds paymentId offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just refundList -> Right refundList
            Nothing         -> Left $ ParseFailure rawBody
        404 -> Left NotFound
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = (Text.intercalate "/" [paymentsPath, paymentId, refundsPath]) <> query
        query = "?offset=" <> (Text.pack $ show offset) <> "&count=" <> (Text.pack $ show count)

{-|
  Handler to get a list of refunds. Because the list endpoint is paginated this handler requires an offset and a count. The maximum amount of refunds returned with a single call is 250.

  For more information see: https://www.mollie.com/en/docs/reference/refunds/list-all.
-}
getRefunds :: Int -> Int -> Mollie (Either Failure (List Refund))
getRefunds offset count = do
    (statusCode, rawBody) <- get path
    return $ case statusCode of
        200 -> case Aeson.decode rawBody of
            Just refundList -> Right refundList
            Nothing         -> Left $ ParseFailure rawBody
        _   -> Left $ RequestFailure statusCode rawBody
    where
        path = refundsPath <> query
        query = "?offset=" <> (Text.pack $ show offset) <> "&count=" <> (Text.pack $ show count)
