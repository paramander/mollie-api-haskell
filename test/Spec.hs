import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.List
import           Data.Ord

import           Control.Lens             (view, (&), (.~), (^.))
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Currency            as Currency
import           Data.Default             (def)
import qualified Data.Time                as Time
import qualified Fixtures
import qualified Mollie.API.Chargebacks   as Chargebacks
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
  , chargebackTests
  ]

paymentTests = testGroup "Payment tests"
  [ testCase "Payment list count" $ do
      paymentsList <- Fixtures.payments
      paymentsList ^. Mollie.count @?= 5

  , testCase "Payment fields" $ do
      [payment] <- fmap (view Mollie.embedded) Fixtures.payments
      payment ^. Payments.status @?= Payments.PaymentOpen
      payment ^. Payments.mode @?= Mollie.Test
      payment ^. Payments.isCancelable @?= False
      payment ^. Payments.amount @?= (def & Mollie.value .~ "75.00" & Mollie.currency .~ Currency.GBP)
      payment ^. Payments.method @?= Just Payments.Ideal
  ]

customerTests = testGroup "Customer tests"
  [ testCase "Customer list count" $ do
      customersList <- Fixtures.customers
      customersList ^. Mollie.count @?= 1
  , testCase "Customer fields" $ do
      [customer] <- fmap (view Mollie.embedded) Fixtures.customers
      customer ^. Customers.name  @?= Just "Customer A"
      customer ^. Customers.email @?= Just "customer@example.org"
      customer ^. Customers.recentlyUsedMethods @?= [Payments.Creditcard, Payments.Ideal]
  ]

refundTests = testGroup "Refund tests"
  [ testCase "Refund list count" $ do
      refundsList <- Fixtures.refunds
      refundsList ^. Mollie.count @?= 5
  , testCase "Refund fields" $ do
      [refund] <- fmap (view Mollie.embedded) Fixtures.refunds
      refund ^. Refunds.amount @?= (def & Mollie.value .~ "5.95")
      refund ^. Refunds.status @?= Refunds.RefundPending
      refund ^. Refunds.paymentId @?= "tr_WDqYK6vllg"
  ]

subscriptionTests = testGroup "Subscription tests"
  [ testCase "Subscription list count" $ do
      subscriptionsList <- Fixtures.subscriptions
      subscriptionsList ^. Mollie.count @?= 3
  , testCase "Subscription fields" $ do
      [subscription] <- fmap (view Mollie.embedded) Fixtures.subscriptions
      subscription ^. Subscriptions.mode @?= Mollie.Live
      subscription ^. Subscriptions.status @?= Subscriptions.SubscriptionActive
      subscription ^. Subscriptions.amount @?= (def & Mollie.value .~ "25.00")
      subscription ^. Subscriptions.times @?= Just 4
      subscription ^. Subscriptions.interval @?= "3 months"
      subscription ^. Subscriptions.method @?= Just Payments.Creditcard
  ]

mandateTests = testGroup "Mandate tests"
  [ testCase "Mandate list count" $ do
      mandatesList <- Fixtures.mandates
      mandatesList ^. Mollie.count @?= 1
  , testCase "Mandate fields" $ do
      [mandate] <- fmap (view Mollie.embedded) Fixtures.mandates
      mandate ^. Mandates.status @?= Mandates.MandateValid
      mandate ^. Mandates.method @?= Payments.Directdebit
      mandate ^. Mandates.mandateReference @?= Nothing
      mandate ^. Mandates.signatureDate @?= Just "2018-05-07"
  , testCase "Mandate details" $ do
      [mandate] <- fmap (view Mollie.embedded) Fixtures.mandates
      let Just details = mandate ^. Mandates.details

      details ^. Mandates.consumerName @?= Just "John Doe"
      details ^. Mandates.consumerAccount @?= Just "NL55INGB0000000000"
      details ^. Mandates.consumerBic @?= Just "INGBNL2A"
  ]

chargebackTests = testGroup "Chargeback tests"
  [ testCase "Chargeback list count" $ do
      chargebackList <- Fixtures.chargebacks
      chargebackList ^. Mollie.count @?= 1
  , testCase "Chargeback fields" $ do
      [chargeback] <- fmap (view Mollie.embedded) Fixtures.chargebacks
      -- Resolves to 2018-03-14T17:00:52
      let time = Time.UTCTime (Time.fromGregorian 2018 3 14) (17*60*60 + 0*60 + 52)
      chargeback ^. Chargebacks.amount @?= (def & Mollie.value .~ "43.38" & Mollie.currency .~ Currency.USD)
      chargeback ^. Chargebacks.settlementAmount @?= Just (def & Mollie.value .~ "35.07")
      chargeback ^. Chargebacks.createdAt @?= time
      chargeback ^. Chargebacks.reversedAt @?= Nothing
      chargeback ^. Chargebacks.paymentId @?= "tr_WDqYK6vllg"
  ]
