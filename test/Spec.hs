import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.List
import           Data.Ord

import           Control.Lens             (view, (&), (.~))
import qualified Data.Currency            as Currency
import           Data.Default             (def)
import qualified Fixtures
import qualified Mollie.API.Customers     as Customers
import qualified Mollie.API.Mandates      as Mandates
import qualified Mollie.API.Payments      as Payments
import qualified Mollie.API.Refunds       as Refunds
import qualified Mollie.API.Subscriptions as Subscriptions
import qualified Mollie.API.Types         as Mollie

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
      view Mollie.count paymentsList @?= 5

  , testCase "Payment fields" $ do
      [payment] <- fmap (view Mollie.embedded) Fixtures.payments
      view Payments.status payment @?= Payments.PaymentOpen
      view Payments.mode payment @?= Mollie.Test
      view Payments.isCancelable payment @?= False
      view Payments.amount payment @?= (def & Mollie.value .~ "75.00" & Mollie.currency .~ Currency.GBP)
      view Payments.method payment @?= Just Payments.Ideal
  ]

customerTests = testGroup "Customer tests"
  [ testCase "Customer list count" $ do
      customersList <- Fixtures.customers
      view Mollie.count customersList @?= 1
  , testCase "Customer fields" $ do
      [customer] <- fmap (view Mollie.embedded) Fixtures.customers
      view Customers.name customer @?= Just "Customer A"
      view Customers.email customer @?= Just "customer@example.org"
      view Customers.recentlyUsedMethods customer @?= [Payments.Creditcard, Payments.Ideal]
  ]

refundTests = testGroup "Refund tests"
  [ testCase "Refund list count" $ do
      refundsList <- Fixtures.refunds
      view Mollie.count refundsList @?= 5
  , testCase "Refund fields" $ do
      [refund] <- fmap (view Mollie.embedded) Fixtures.refunds
      view Refunds.amount refund @?= (def & Mollie.value .~ "5.95")
      view Refunds.status refund @?= Refunds.RefundPending
      view Refunds.paymentId refund @?= "tr_WDqYK6vllg"
  ]

subscriptionTests = testGroup "Subscription tests"
  [ testCase "Subscription list count" $ do
      subscriptionsList <- Fixtures.subscriptions
      view Mollie.count subscriptionsList @?= 3
  , testCase "Subscription fields" $ do
      [subscription] <- fmap (view Mollie.embedded) Fixtures.subscriptions
      view Subscriptions.mode subscription @?= Mollie.Live
      view Subscriptions.status subscription @?= Subscriptions.SubscriptionActive
      view Subscriptions.amount subscription @?= (def & Mollie.value .~ "25.00")
      view Subscriptions.times subscription @?= Just 4
      view Subscriptions.interval subscription @?= "3 months"
      view Subscriptions.method subscription @?= Just Payments.Creditcard
  ]

mandateTests = testGroup "Mandate tests"
  [ testCase "Mandate list count" $ do
      mandatesList <- Fixtures.mandates
      view Mollie.count mandatesList @?= 1
  , testCase "Mandate fields" $ do
      [mandate] <- fmap (view Mollie.embedded) Fixtures.mandates
      view Mandates.status mandate @?= Mandates.MandateValid
      view Mandates.method mandate @?= Payments.Directdebit
      view Mandates.mandateReference mandate @?= Nothing
      view Mandates.signatureDate mandate @?= Just "2018-05-07"
  , testCase "Mandate details" $ do
      [mandate] <- fmap (view Mollie.embedded) Fixtures.mandates
      let Just details = view Mandates.details mandate

      view Mandates.consumerName details @?= Just "John Doe"
      view Mandates.consumerAccount details @?= Just "NL55INGB0000000000"
      view Mandates.consumerBic details @?= Just "INGBNL2A"
  ]
