{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text           as Text
import           Mollie.API
import           Mollie.API.Methods
import           Mollie.API.Payments
import           Mollie.API.Refunds
import           Mollie.API.Issuers
import           Mollie.API.Customers
import           Mollie.API.Mandates
import           Mollie.API.Subscriptions
import qualified System.Environment  as Environment

main :: IO ()
main = do
    key <- fmap Text.pack $ Environment.getEnv "MOLLIE_API_KEY"
    env <- createEnv key
    runMollie env $ do
        -- Right payment1 <- createPayment (newPayment 149.95 "Test payment" "http://localhost:3000/")
        --     { newPayment_method = Just Ideal
        --     }
        -- Right _payment2 <- getPayment (payment_id payment1)
        -- Right paymentList <- getPayments 0 10
        -- Right refund1 <- createPaymentRefund "tr_FgywfCctVw" newRefund
        --     { newRefund_description = Just "Refund test payment"
        --     }
        -- Right _refund2 <- getPaymentRefund "tr_FgywfCctVw" (refund_id refund1)
        -- Nothing <- cancelPaymentRefund (payment_id payment1) (refund_id refund1)
        -- paymentRefundList <- getPaymentRefunds "tr_FgywfCctVw" 0 10
        -- refundList <- getRefunds 0 10
        -- Right methodList <- getMethods "en" 0 250
        -- Right method <- getMethod Ideal "en"
        -- Right issuerList <- getIssuers 0 250
        -- Right issuer <- getIssuer "ideal_TESTNL99"
        -- Right customer <- createCustomer (newCustomer "Test Customer" "test+mollie@paramander.com")
        -- Right customer <- getCustomer "cst_kM5sMuBQst"
        -- Right customerList <- getCustomers 0 250
        -- Right payment <- createCustomerPayment "cst_kM5sMuBQst" (newPayment 20 "Description" "http://localhost:3000")
        -- Right payment <- createPayment (newPayment 10 "Description" "http://localhost:3000")
        --     { newPayment_customerId = Just "cst_kM5sMuBQst"
        --     }
        -- Right customerPaymentList <- getCustomerPayments "cst_kM5sMuBQst" 0 250
        -- Right mandate <- createCustomerMandate "cst_kM5sMuBQst" (newMandate Directdebit "Test customer" "NL73ABNA0523040997")
        -- Right mandate <- getCustomerMandate "cst_kM5sMuBQst" "mdt_FeC3qN38kk"
        -- Right mandateList <- getCustomerMandates "cst_kM5sMuBQst" 0 250
        -- Right payment <- createCustomerPayment "cst_kM5sMuBQst" (newPayment 0.01 "Initial payment" "http://localhost:3000")
        --     { newPayment_recurringType = Just First
        --     }
        -- Right subscription <- createCustomerSubscription "cst_kM5sMuBQst" (newSubscription 16.95 "1 month" "Montly")
        -- Right subscription <- getCustomerSubscription "cst_kM5sMuBQst" "sub_qkSa5k5z3q"
        -- Right subscriptionList <- getCustomerSubscriptions "cst_kM5sMuBQst" 0 250
        Right subscription <- cancelCustomerSubscription "cst_kM5sMuBQst" "sub_qkSa5k5z3q"
        liftIO $ putStrLn $ show subscription
        return ()
