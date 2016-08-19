{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import           Data.Text            as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy       as TL
import           Mollie.API
import           System.Environment
import           Web.Scotty.Trans
-- new payment
import           Data.Aeson
import           Data.Monoid
import qualified Data.UUID            as UUID
import qualified Data.UUID.V4         as UUID
import           Mollie.API.Payments
import           Network.Wai.Request
-- webhook verification
import qualified Data.HashMap.Strict  as HM
import           System.Directory
-- return page
import           Mollie.API.Types
-- ideal payment
import           Lucid.Base
import           Lucid.Html5
import           Mollie.API.Issuers

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
        get "/ideal-payment" getIdealPaymentHandler
        post "/ideal-payment" postIdealPaymentHandler
        notFound (text "Page not found")

withMollie :: Mollie a -> Handler a
withMollie query = do
    mollieEnv <- lift $ ask
    liftIO $ runMollie mollieEnv query

newPaymentHandler :: Handler ()
newPaymentHandler = do
    -- Generate a unique identifier.
    orderId <- fmap UUID.toText $ liftIO $ UUID.nextRandom

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
            -- Read the status from the file as `PaymentStatus` and display it.
            -- In production you would probably want to read it from a database.
            -- Note that the contents of the file are trusted and we do only
            -- expect it to contain a valid `PaymentStatus`.
            paymentStatus <- liftIO $ fmap read $ readFile filepath :: Handler PaymentStatus
            text $ "Payment status for your order: " <> (TL.fromStrict $ toText paymentStatus)
        else text "Order not found."

getIdealPaymentHandler :: Handler ()
getIdealPaymentHandler = do
    -- Get the first 250 issuers (there will probably be less).
    -- In production you would want to check if there are more.
    i <- withMollie $ getIssuers 0 250
    case i of
        Right issuersList -> do
            -- Extract all ideal issuers for the list and display them in a form.
            let issuers = list_data issuersList
                idealIssuers = Prelude.filter ((==) Ideal . issuer_method) issuers
                optionTag :: Issuer -> Html ()
                optionTag issuer = option_
                    [ value_ (issuer_id issuer) ]
                    (toHtml $ issuer_name issuer)

            html $ renderText $ do
                form_ [ method_ "post" ] $ do
                    label_ "Select your bank:"
                    select_ [ name_ "issuer" ] $ do
                        mapM_ optionTag idealIssuers
                        option_ [ value_ "" ] "or select later"
                    button_ "OK"
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> (pack $ show err)

postIdealPaymentHandler :: Handler ()
postIdealPaymentHandler = do
    -- Get the selected issuer and check it for being empty.
    -- In production your should check if this value is actually
    -- a possible issuer for iDEAL, always check user input!
    i <- param "issuer"
    let issuer = if Text.null i then Nothing else Just i

    -- Generate a unique identifier.
    orderId <- fmap UUID.toText $ liftIO $ UUID.nextRandom

    -- Determine the url for these examples.
    hostUrl <- fmap (decodeUtf8 . guessApproot) $ request

    -- Create the actual payment in Mollie.
    p <- withMollie $ createPayment
        (newPayment 27.50 "My first iDEAL payment" (hostUrl <> "/return-page?order_id=" <> orderId))
        { newPayment_webhookUrl = Just (hostUrl <> "/webhook-verification")
        , newPayment_metadata   = Just $ object ["order_id" .= orderId]
        , newPayment_method     = Just Ideal
        , newPayment_issuer     = issuer
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
