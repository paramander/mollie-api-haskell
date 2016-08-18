{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import           Data.Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy       as TL
import           Mollie.API
import           System.Environment
import           Web.Scotty.Trans
-- new payment
import           Data.Aeson
import           Data.Monoid
import           Data.UUID
import           Data.UUID.V4
import           Mollie.API.Payments
import           Network.Wai.Request
-- webhook verification
import qualified Data.HashMap.Strict  as HM
import           System.Directory

type Handler a = ActionT TL.Text (ReaderT Env IO) a

main :: IO ()
main = do
    mollieKey <- fmap pack $ getEnv "MOLLIE_API_KEY"
    mollieEnv <- createEnv mollieKey
    let r m = runReaderT m mollieEnv
    scottyT 3000 r $ do
        get "/new-payment" newPaymentHandler
        post "/webhook-verification" webhookVerificationHandler
        get "/return-page" returnPageHandler
        notFound (text "Page not found")

withMollie :: Mollie a -> Handler a
withMollie query = do
    mollieEnv <- lift $ ask
    liftIO $ runMollie mollieEnv query

newPaymentHandler :: Handler ()
newPaymentHandler = do
    -- Generate a unique identifier.
    orderId <- fmap toText $ liftIO $ nextRandom

    -- Determine the url for these examples.
    hostUrl <- fmap (decodeUtf8 . guessApproot) $ request

    -- Create the actual payment in Mollie.
    p <- withMollie $ createPayment
        (newPayment 10.00 "My first API payment" (hostUrl <> "/return-page?order_id=" <> orderId))
        { newPayment_webhookUrl = Just (hostUrl <> "/webhook-verification")
        , newPayment_metadata   = Just $ object ["order_id" .= orderId]
        }

    case p of
        Right payment -> do
            -- Write the status to a file. In production you should probably use a
            -- database.
            liftIO $ writeFile (unpack $ "./orders/order-" <> orderId <> ".txt") (show $ payment_status payment)

            -- This payment should always have a redirect url as we did not create
            -- a recurring payment. So we can savely pattern match on a `Just` and
            -- redirect the user to the checkout screen.
            let Just redirectUrl = paymentLinks_paymentUrl $ payment_links payment
            redirect $ TL.fromStrict redirectUrl
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> (pack $ show err)

webhookVerificationHandler :: Handler ()
webhookVerificationHandler = do
    -- The id param should be present.
    paymentId <- param "id"

    -- Fetch the payment from Mollie.
    p <- withMollie $ getPayment paymentId

    case p of
        Right payment -> do
            -- In this example we only anticipate payments which do have metadata
            -- containing a json object with a custom order id as text.
            let Just (Object metadata) = payment_metadata payment
                Just (String orderId) = HM.lookup "order_id" metadata

            -- Update our local record for this payment with the new status.
            liftIO $ writeFile (unpack $ "./orders/order-" <> orderId <> ".txt") (show $ payment_status payment)

            -- Here we would probably want to check the new status and act on it.
            -- For instance when the status changed to `PaymentPaid` we should
            -- ship the product.

            -- At this point we should simply return a 200 code.
            text "success"
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> (pack $ show err)

returnPageHandler :: Handler ()
returnPageHandler = do
    -- Lookup the order_id param or 404 when its not there.
    o <- fmap (lookup "order_id") params
    orderId <- maybe next return o

    let filepath = TL.unpack $ "./orders/order-" <> orderId <> ".txt"

    -- Check if a file for this order exists.
    fileExists <- liftIO $ doesFileExist filepath
    if fileExists
        then do
            -- Read the status from the file as `String` and display it. In
            -- production you would probably want to read it from a database
            -- as an actual `PaymentStatus`.
            paymentStatus <- liftIO $ readFile filepath
            text $ "Payment status for your order: " <> TL.pack paymentStatus
        else text "Order not found."
