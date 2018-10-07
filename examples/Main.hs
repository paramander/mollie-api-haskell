{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Main where

import           Control.Lens             hiding ((.=))
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              hiding (First)
import           Data.Text                as Text
import           Data.Text.Encoding
import qualified Data.Text.Lazy           as TL
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID
import           Lucid.Base
import           Lucid.Html5
import           Mollie.API
import qualified Mollie.API.Customers     as Customers
import qualified Mollie.API.Issuers       as Issuers
import qualified Mollie.API.Methods       as Methods
import qualified Mollie.API.Payments      as Payments
import qualified Mollie.API.Refunds       as Refunds
import qualified Mollie.API.Subscriptions as Subscriptions
import           Mollie.API.Types         hiding (next)
import           Network.Wai.Request
import           System.Directory
import           System.Environment
import           Web.Scotty.Trans

type Handler a = ActionT TL.Text (ReaderT Env IO) a

main :: IO ()
main = do
    mollieKey <- pack <$> getEnv "MOLLIE_API_KEY"
    mollieEnv <- createEnv mollieKey
    let r m = runReaderT m mollieEnv
    scottyT 3000 r $ do
        get "/new-payment" newPaymentHandler
        post "/webhook-verification" webhookVerificationHandler
        get "/return-page" returnPageHandler
        get "/ideal-payment" getIdealPaymentHandler
        post "/ideal-payment" postIdealPaymentHandler
        get "/payments-history" paymentsHistoryHandler
        get "/list-activated-methods" listActivatedMethodsHandler
        get "/refund-payment" refundPaymentHandler
        get "/list-customers" getCustomersHandler
        get "/new-customer" newCustomerHandler
        get "/new-customer-payment" newCustomerPaymentHandler
        get "/customer-payments-history" customerPaymentsHistoryHandler
        get "/recurring-first-payment" recurringFirstPaymentHandler
        get "/recurring-payment" recurringPaymentHandler
        get "/recurring-subscription" recurringSubscriptionHandler
        get "/cancel-subscription" cancelSubscriptionHandler
        notFound (text "Page not found")

withMollie :: Mollie a -> Handler a
withMollie query = do
    mollieEnv <- lift ask
    liftIO $ runMollie mollieEnv query

newPaymentHandler :: Handler ()
newPaymentHandler = do
    -- Generate a unique identifier.
    orderId <- UUID.toText <$> liftIO UUID.nextRandom

    -- Determine the url for these examples.
    hostUrl <- fmap (decodeUtf8 . guessApproot) request

    -- Create the actual payment in Mollie.
    p <- withMollie $ Payments.createPayment $
        (Payments.newPayment 10.00 "My first API payment" (hostUrl <> "/return-page?order_id=" <> orderId))
        & Payments.metadata .~ (Just $ object ["order_id" .= orderId])

    case p of
        Right payment -> do
            -- Write the status to a file. In production you should probably use a
            -- database.
            liftIO $ writeFile (unpack $ "./orders/order-" <> orderId <> ".txt") (show $ view Payments.status payment)

            -- This payment should always have a redirect url as we did not create
            -- a recurring payment. So we can savely pattern match on a `Just` and
            -- redirect the user to the checkout screen.
            let Just redirectUrl = view Payments.redirectUrl payment
            redirect $ TL.fromStrict redirectUrl
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

webhookVerificationHandler :: Handler ()
webhookVerificationHandler = do
    -- The id param should be present.
    paymentId <- param "id"

    -- Fetch the payment from Mollie.
    p <- withMollie $ Payments.getPayment paymentId

    case p of
        Right payment -> do
            -- In this example we only anticipate payments which do have metadata
            -- containing a json object with a custom order id as text.
            let Just (Object metadata) = view Payments.metadata payment
                Just (String orderId) = HM.lookup "order_id" metadata

            -- Update our local record for this payment with the new status.
            liftIO $ writeFile (unpack $ "./orders/order-" <> orderId <> ".txt") (show $ view Payments.status payment)

            -- Here we would probably want to check the new status and act on it.
            -- For instance when the status changed to `PaymentPaid` we should
            -- ship the product.

            -- At this point we should simply return a 200 code.
            text "success"
        Left (ClientError 404 _) -> next
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

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
            paymentStatus <- liftIO $ read <$> readFile filepath :: Handler Payments.PaymentStatus
            text $ "Payment status for your order: " <> TL.fromStrict (toText paymentStatus)
        else text "Order not found."

-- TODO: Issuers are embedded inside Methods api.
getIdealPaymentHandler :: Handler ()
getIdealPaymentHandler = do
    -- Get the first 250 issuers (there will probably be less).
    -- In production you would want to check if there are more.
    i <- withMollie $ Issuers.getIssuers [queryParam "limit" "250"]
    case i of
        Right issuersList -> do
            -- Extract all ideal issuers for the list and display them in a form.
            let issuers = view embedded issuersList
                idealIssuers = Prelude.filter ((==) Methods.Ideal . (view Issuers.method)) issuers
                optionTag :: Issuers.Issuer -> Html ()
                optionTag issuer = option_
                    [ value_ (view Issuers.id issuer) ]
                    (toHtml $ view Issuers.name issuer)

            html $ renderText $
                form_ [ method_ "post" ] $ do
                    label_ "Select your bank:"
                    select_ [ name_ "issuer" ] $ do
                        mapM_ optionTag idealIssuers
                        option_ [ value_ "" ] "or select later"
                    button_ "OK"
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

postIdealPaymentHandler :: Handler ()
postIdealPaymentHandler = do
    -- Get the selected issuer and check it for being empty.
    -- In production your should check if this value is actually
    -- a possible issuer for iDEAL, always check user input!
    i <- param "issuer"
    let issuer = if Text.null i then Nothing else Just i

    -- Generate a unique identifier.
    orderId <- UUID.toText <$> liftIO UUID.nextRandom

    -- Determine the url for these examples.
    hostUrl <- fmap (decodeUtf8 . guessApproot) request

    -- Create the actual payment in Mollie, forcing the method
    -- to iDEAL and settings the selected issuer.
    p <- withMollie $ Payments.createPayment $
        (Payments.newPayment 27.50 "My first iDEAL payment" (hostUrl <> "/return-page?order_id=" <> orderId))
        & Payments.metadata .~ (Just $ object ["order_id" .= orderId])
        & Payments.method   .~ (Just Methods.Ideal)
        & Payments.issuer   .~ issuer

    case p of
        Right payment -> do
            -- Write the status to a file. In production you should probably use a
            -- database.
            liftIO $ writeFile (unpack $ "./orders/order-" <> orderId <> ".txt") (show $ view Payments.status payment)

            -- This payment should always have a redirect url as we did not create
            -- a recurring payment. So we can savely pattern match on a `Just` and
            -- redirect the user to the checkout screen.
            let Just redirectUrl = view Payments.redirectUrl payment
            redirect $ TL.fromStrict redirectUrl
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

paymentsHistoryHandler :: Handler ()
paymentsHistoryHandler = do
    -- Get the latest 25 payments.
    p <- withMollie $ Payments.getPayments [queryParam "limit" "25"]
    case p of
        Right paymentList -> do
            -- Extract the payments from the list and display them.
            let payments = view embedded paymentList
                paymentTag :: Payments.Payment -> Html ()
                paymentTag payment = tr_ $ do
                    td_ (toHtml $ view Payments.id payment)
                    td_ $ do
                        (toHtmlRaw ("&euro;" :: Text))
                        (toHtml $ view (Payments.amount . value) payment)
                    td_ (toHtml $ toText $ view Payments.status payment)

            html $ renderText $
                table_ $ do
                    thead_ $ tr_ $ do
                        th_ "ID"
                        th_ "Amount"
                        th_ "Status"
                    tbody_ $
                        mapM_ paymentTag payments
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

getCustomersHandler :: Handler ()
getCustomersHandler = do
    p <- withMollie $ Customers.getCustomers [queryParam "limit" "250"]
    case p of
        Right customerList -> do
            let customers = view embedded customerList
                customerTag :: Customers.Customer -> Html ()
                customerTag customer = tr_ $ do
                    td_ (toHtml $ view Customers.id customer)
                    td_ (toHtml $ fromMaybe "-" $ view Customers.email customer)
            html $ renderText $
                table_ $ do
                    thead_ $ tr_ $ do
                        th_ "ID"
                        th_ "Email"
                    tbody_ $
                        mapM_ customerTag customers
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

listActivatedMethodsHandler :: Handler ()
listActivatedMethodsHandler = do
    -- Get the first 250 payment methods (there will probably be less).
    -- In production you would want to check if there are more.
    m <- withMollie $ Methods.getMethods [ queryParam "locale" "nl_NL"]
    case m of
        Right methodList -> do
            -- Extract all methods from the list and display them along
            -- with the total amount of methods available.
            let methods = view embedded methodList
                methodTag :: Methods.Method -> Html ()
                methodTag method = div_ [ style_ "line-height:32px; vertical-align:top" ] $ do
                    img_ [ src_ (view (Methods.image . Methods.size1x) method) ]
                    p_ (toHtml $ view Methods.description method <> " (" <> toText (view Methods.id method) <> ")")

            html $ renderText $ do
                p_ (toHtml $ "Your API key has " <> pack (show $ view Mollie.API.Types.count methodList) <> " activated payment methods:")
                mapM_ methodTag methods
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

refundPaymentHandler :: Handler ()
refundPaymentHandler = do
    -- Get the payment by id.
    paymentId <- param "payment_id"
    p <- withMollie $ Payments.getPayment paymentId
    case p of
        Right payment -> do
            let mAmountRemaining = view Payments.amountRemaining payment
            -- Check if there is an amount remaining to refund. If there is
            -- We refund it all. Otherwise we notify the user this payment
            -- can't be refunded.
            case mAmountRemaining of
                Just amount_ | read (unpack $ view value amount_) > (0 :: Double) -> do
                    r <- withMollie $ Refunds.createPaymentRefund paymentId $
                        (Refunds.newRefund & Refunds.amount .~ (Just $ defaultAmount $ read $ unpack $ view value amount_))
                    case r of
                        Right _refund -> text "Payment refunded."
                        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)
                _ -> text "This payment can't be refunded."
        Left (ClientError 404 _) -> next
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

newCustomerHandler :: Handler ()
newCustomerHandler = do
    -- Create a new customer with some metadata.
    c <- withMollie $ Customers.createCustomer $ (Customers.newCustomer "Test customer" "test@example.com")
        & Customers.metadata .~ (Just $ object ["discount" .= True])

    case c of
        Right customer ->
            text $ TL.fromStrict $ "New customer created " <> view Customers.id customer <> " (" <> fromMaybe ("" :: Text.Text) (view Customers.name customer) <> ")"
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

newCustomerPaymentHandler :: Handler ()
newCustomerPaymentHandler = do
    -- Generate a unique identifier.
    orderId <- UUID.toText <$> liftIO UUID.nextRandom

    -- Determine the url for these examples.
    hostUrl <- fmap (decodeUtf8 . guessApproot) request

    -- Create the actual payment in Mollie for the first customer.
    -- Note that we are expecting there to be atleast one existing
    -- customer and ignore any possibility of errors while requesting
    -- that customer from the API.
    p <- withMollie $ do
        Right customerList <- Customers.getCustomers [queryParam "limit" "1"]
        let customerId = view Customers.id $ Prelude.head $ view embedded customerList
        Customers.createCustomerPayment customerId $
            (Payments.newPayment 10.00 "My first Customer payment" (hostUrl <> "/return-page?order_id=" <> orderId))
            & Payments.metadata .~ (Just $ object ["order_id" .= orderId])

    case p of
        Right payment -> do
            -- Write the status to a file. In production you should probably use a
            -- database.
            liftIO $ writeFile (unpack $ "./orders/order-" <> orderId <> ".txt") (show $ view Payments.status payment)

            -- This payment should always have a redirect url as we did not create
            -- a recurring payment. So we can savely pattern match on a `Just` and
            -- redirect the user to the checkout screen.
            let Just redirectUrl = view Payments.redirectUrl payment
            redirect $ TL.fromStrict redirectUrl
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

customerPaymentsHistoryHandler :: Handler ()
customerPaymentsHistoryHandler = do
    -- Retrieve the payment history for the first customer.
    -- Note that we are expecting there to be atleast one existing
    -- customer and ignore any possibility of errors while requesting
    -- that customer from the API.
    l <- withMollie $ do
        Right customerList <- Customers.getCustomers [queryParam "limit" "1"]
        let customerId = view Customers.id $ Prelude.head $ view embedded customerList
        Customers.getCustomerPayments customerId [queryParam "limit" "25"]

    case l of
        Right paymentList -> do
            -- Extract the payments from the list and display them.
            let payments = view embedded paymentList
                paymentTag :: Payments.Payment -> Html ()
                paymentTag payment = tr_ $ do
                    td_ (toHtml $ view Payments.id payment)
                    td_ $ do
                        (toHtmlRaw ("&euro;" :: Text))
                        (toHtml $ view (Payments.amount . value) payment)
                    td_ (toHtml $ toText $ view Payments.status payment)

            html $ renderText $ do
                table_ $ do
                    thead_ $ tr_ $ do
                        th_ "ID"
                        th_ "Amount"
                        th_ "Status"
                    tbody_ $ do
                        mapM_ paymentTag payments
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> pack (show err)

recurringFirstPaymentHandler :: Handler ()
recurringFirstPaymentHandler = do
    -- Generate a unique identifier.
    orderId <- UUID.toText <$> liftIO UUID.nextRandom

    -- Determine the url for these examples.
    hostUrl <- fmap (decodeUtf8 . guessApproot) request

    -- Create the first recurring payment in Mollie for the first customer.
    -- Note that we are expecting there to be atleast one existing
    -- customer and ignore any possibility of errors while requesting
    -- that customer from the API.
    p <- withMollie $ do
        Right customerList <- Customers.getCustomers []
        let customerId = view Customers.id $ Prelude.head $ view embedded customerList
        Customers.createCustomerPayment customerId $
            (Payments.newPayment 10.00 "A first recurring payment" (hostUrl <> "/return-page?order_id=" <> orderId))
              & Payments.metadata     .~ (Just $ object ["order_id" .= orderId])
              & Payments.sequenceType .~ Just Payments.First

    case p of
        Right payment -> do
            -- Write the status to a file. In production you should probably use a
            -- database.
            liftIO $ writeFile (unpack $ "./orders/order-" <> orderId <> ".txt") (show $ view Payments.status payment)

            -- This payment should always have a redirect url as we did not create
            -- a recurring payment. So we can savely pattern match on a `Just` and
            -- redirect the user to the checkout screen.
            let Just redirectUrl = view Payments.redirectUrl payment
            redirect $ TL.fromStrict redirectUrl
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> (pack $ show err)

recurringPaymentHandler :: Handler ()
recurringPaymentHandler = do
    -- Generate a unique identifier.
    orderId <- fmap UUID.toText $ liftIO UUID.nextRandom

    -- Determine the url for these examples.
    hostUrl <- fmap (decodeUtf8 . guessApproot) request

    -- Create the first recurring payment in Mollie for the first customer.
    -- Note that we are expecting there to be atleast one existing
    -- customer and ignore any possibility of errors while requesting
    -- that customer from the API.
    p <- withMollie $ do
        Right customerList <- Customers.getCustomers [queryParam "limit" "1"]
        let customerId = view Customers.id $ Prelude.head $ view embedded customerList
        Customers.createCustomerPayment customerId $
            (Payments.newRecurringPayment 10.00 "An on-demand recurring payment")
            & Payments.metadata .~ (Just $ object ["order_id" .= orderId])

    case p of
        Right payment -> do
            -- Write the status to a file. In production you should probably use a
            -- database.
            liftIO $ writeFile (unpack $ "./orders/order-" <> orderId <> ".txt") (show $ view Payments.status payment)

            -- Because this is a recurring payment it should always have a mandate
            -- id and a method.
            let Just mandateId = view Payments.mandateId payment
                Just method = view Payments.method payment

            text $ TL.fromStrict ("Selected mandate is '" <> mandateId <> "' (" <> toText method <> ")")
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> (pack $ show err)

recurringSubscriptionHandler :: Handler ()
recurringSubscriptionHandler = do
    -- Generate a unique identifier.
    subscriptionId <- UUID.toText <$> liftIO UUID.nextRandom

    -- Determine the url for these examples.
    hostUrl <- fmap (decodeUtf8 . guessApproot) request

    -- Create the subscription in Mollie for the first customer.
    -- Note that we are expecting there to be atleast one existing
    -- customer with a valid mandate and ignore any possibility of
    -- errors while requesting that customer from the API.
    s <- withMollie $ do
        Right customerList <- Customers.getCustomers [queryParam "limit" "1"]
        let customerId = view Customers.id $ Prelude.head $ view embedded customerList
        Subscriptions.createCustomerSubscription customerId $
            (Subscriptions.newSubscription 10.00 "1 month" "My subscription")
            & Subscriptions.times .~ Just 12
            & Subscriptions.webhookUrl .~ Nothing

    case s of
        Right subscription -> do
            html $ renderText $ do
                p_ (toHtml $ "The subscription status: " <> (toText $ view Subscriptions.status subscription))
                p_ $ do
                    a_ [ href_ (hostUrl <> "/cancel-subscription?Subscriptions.id=" <> view Subscriptions.id subscription)] "cancel subscription"
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> (pack $ show err)

cancelSubscriptionHandler :: Handler ()
cancelSubscriptionHandler = do
    subscriptionId <- param "Subscriptions.id"

    -- Cancle the subscription for the first customer.
    -- Note that we are expecting there to be atleast one existing
    -- customer with a valid mandate and ignore any possibility of
    -- errors while requesting that customer from the API.
    s <- withMollie $ do
        Right customerList <- Customers.getCustomers [queryParam "limit" "1"]
        let customerId = view Customers.id $ Prelude.head $ view embedded customerList
        Subscriptions.cancelCustomerSubscription customerId subscriptionId

    case s of
        Right subscription -> do
            html $ renderText $ do
                p_ (toHtml $ "The subscription status: " <> (toText $ view Subscriptions.status subscription))
        Left (ClientError 404 _) -> next
        Left err -> raise $ TL.fromStrict $ "API call failed: " <> (pack $ show err)
