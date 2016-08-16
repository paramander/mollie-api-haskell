{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text            as Text
import           Mollie.API
import           Mollie.API.Payments
import           Mollie.API.Refunds
import qualified System.Environment   as Environment

main :: IO ()
main = do
    key <- fmap Text.pack $ Environment.getEnv "MOLLIE_API_KEY"
    env <- createEnv key
    let run = runMollie env

    (payments, refunds) <- run $ do
        -- Right payment1 <- createPayment (newPayment 149.95 "Test payment" "http://localhost:3000/")
        --     { newPayment_method = Just Ideal
        --     }
        -- Right _payment2 <- getPayment (payment_id payment1)
        Right paymentList <- getPayments 0 10
        -- Right refund1 <- createPaymentRefund "tr_FgywfCctVw" newRefund
        --     { newRefund_description = Just "Refund test payment"
        --     }
        -- Right _refund2 <- getPaymentRefund "tr_FgywfCctVw" (refund_id refund1)
        -- Nothing <- cancelPaymentRefund (payment_id payment1) (refund_id refund1)
        -- paymentRefundList <- getPaymentRefunds "tr_FgywfCctVw" 0 10
        refundList <- getRefunds 0 10
        return (paymentList, refundList)

    putStrLn $ show payments
    -- putStrLn $ show paymentRefunds
    putStrLn $ show refunds
