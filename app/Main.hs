{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text            as Text
import           Mollie.API
import           Mollie.API.Payments
import qualified System.Environment   as Environment

main :: IO ()
main = do
    key <- fmap Text.pack $ Environment.getEnv "MOLLIE_API_KEY"
    env <- createEnv key
    let run = runMollie env

    list <- run $ do
        Right payment1 <- createPayment $ NewPayment 149.95 "Test payment" "http://localhost:3000/" Nothing Nothing Nothing Nothing Nothing
        Right _payment2 <- getPayment $ payment_id payment1
        Right paymentList <- getPayments 0 10
        return paymentList

    putStrLn $ show list
