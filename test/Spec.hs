import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.List
import           Data.Ord

import qualified Data.Currency            as Currency
import qualified Fixtures
import qualified Mollie.API.Customers     as Customers
import qualified Mollie.API.Mandates      as Mandates
import qualified Mollie.API.Payments      as Payments
import qualified Mollie.API.Refunds       as Refunds
import qualified Mollie.API.Subscriptions as Subscriptions

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ paymentTests
  , customerTests
  , refundTests
  , subscriptionTests
  , mandateTests
  ]

paymentTests = testGroup "Payment tests"
  [ testCase "Payment list count" $ do
      paymentsList <- Fixtures.payments
      Payments.list_count paymentsList @?= 5

  , testCase "Payment fields" $ do
      [payment] <- fmap Payments.list__embedded Fixtures.payments
      Payments.payment_status payment @?= Payments.PaymentOpen
      Payments.payment_mode payment @?= Payments.Test
      Payments.payment_isCancelable payment @?= False
      Payments.payment_amount payment @?= Payments.Amount { Payments.amount_currency = Currency.GBP, Payments.amount_value = "75.00" }
      Payments.payment_method payment @?= Just Payments.Ideal
  ]

customerTests = testGroup "Customer tests"
  [ testCase "Customer list count" $ do
      customersList <- Fixtures.customers
      Customers.list_count customersList @?= 1
  , testCase "Customer fields" $ do
      [customer] <- fmap Customers.list__embedded Fixtures.customers
      Customers.customer_name customer @?= Just "Customer A"
      Customers.customer_email customer @?= Just "customer@example.org"
      Customers.customer_recentlyUsedMethods customer @?= [Payments.Creditcard, Payments.Ideal]
  ]

refundTests = testGroup "Refund tests"
  [ testCase "Refund list count" $ do
      refundsList <- Fixtures.refunds
      Refunds.list_count refundsList @?= 5
  , testCase "Refund fields" $ do
      [refund] <- fmap Refunds.list__embedded Fixtures.refunds
      Refunds.refund_amount refund @?= Refunds.Amount { Refunds.amount_currency = Currency.EUR, Refunds.amount_value = "5.95" }
      Refunds.refund_status refund @?= Refunds.RefundPending
      Refunds.refund_paymentId refund @?= "tr_WDqYK6vllg"
  ]

subscriptionTests = testGroup "Subscription tests"
  [ testCase "Subscription list count" $ do
      subscriptionsList <- Fixtures.subscriptions
      Subscriptions.list_count subscriptionsList @?= 3
  , testCase "Subscription fields" $ do
      [subscription] <- fmap Subscriptions.list__embedded Fixtures.subscriptions
      Subscriptions.subscription_mode subscription @?= Subscriptions.Live
      Subscriptions.subscription_status subscription @?= Subscriptions.SubscriptionActive
      Subscriptions.subscription_amount subscription @?= Subscriptions.Amount { Subscriptions.amount_currency = Currency.EUR, Subscriptions.amount_value = "25.00" }
      Subscriptions.subscription_times subscription @?= Just 4
      Subscriptions.subscription_interval subscription @?= "3 months"
      Subscriptions.subscription_method subscription @?= Just Subscriptions.Creditcard
  ]

mandateTests = testGroup "Mandate tests"
  [ testCase "Mandate list count" $ do
      mandatesList <- Fixtures.mandates
      Mandates.list_count mandatesList @?= 1
  , testCase "Mandate fields" $ do
      [mandate] <- fmap Mandates.list__embedded Fixtures.mandates
      Mandates.mandate_status mandate @?= Mandates.MandateValid
      Mandates.mandate_method mandate @?= Mandates.Directdebit
      Mandates.mandate_mandateReference mandate @?= Nothing
      Mandates.mandate_signatureDate mandate @?= Just "2018-05-07"
  , testCase "Mandate details" $ do
      [mandate] <- fmap Mandates.list__embedded Fixtures.mandates
      let Just details = Mandates.mandate_details mandate

      Mandates.mandateDetails_consumerName details @?= Just "John Doe"
      Mandates.mandateDetails_consumerAccount details @?= Just "NL55INGB0000000000"
      Mandates.mandateDetails_consumerBic details @?= Just "INGBNL2A"
  ]
