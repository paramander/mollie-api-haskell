import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.List
import           Data.Ord

import           Control.Lens           (view, (&), (.~), (^.))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Currency          as Currency
import           Data.Default           (def)
import qualified Data.Time              as Time
import qualified Fixtures
import           Mollie.API.Types

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
      paymentsList ^. count @?= 5

  , testCase "Payment fields" $ do
      [payment] <- fmap (view embedded) Fixtures.payments
      payment ^. status @?= PaymentOpen
      payment ^. mode @?= Test
      payment ^. isCancelable @?= False
      payment ^. amount @?= (def & value .~ "75.00" & currency .~ Currency.GBP)
      payment ^. method @?= Just Ideal
  ]

customerTests = testGroup "Customer tests"
  [ testCase "Customer list count" $ do
      customersList <- Fixtures.customers
      customersList ^. count @?= 1
  , testCase "Customer fields" $ do
      [customer] <- fmap (view embedded) Fixtures.customers
      customer ^. name  @?= Just "Customer A"
      customer ^. email @?= Just "customer@example.org"
      customer ^. recentlyUsedMethods @?= [Creditcard, Ideal]
  ]

refundTests = testGroup "Refund tests"
  [ testCase "Refund list count" $ do
      refundsList <- Fixtures.refunds
      refundsList ^. count @?= 5
  , testCase "Refund fields" $ do
      [refund] <- fmap (view embedded) Fixtures.refunds
      refund ^. amount @?= (def & value .~ "5.95")
      refund ^. status @?= RefundPending
      refund ^. paymentId @?= "tr_WDqYK6vllg"
  ]

subscriptionTests = testGroup "Subscription tests"
  [ testCase "Subscription list count" $ do
      subscriptionsList <- Fixtures.subscriptions
      subscriptionsList ^. count @?= 3
  , testCase "Subscription fields" $ do
      [subscription] <- fmap (view embedded) Fixtures.subscriptions
      subscription ^. mode @?= Live
      subscription ^. status @?= SubscriptionActive
      subscription ^. amount @?= (def & value .~ "25.00")
      subscription ^. times @?= Just 4
      subscription ^. interval @?= "3 months"
      subscription ^. method @?= Just Creditcard
  ]

mandateTests = testGroup "Mandate tests"
  [ testCase "Mandate list count" $ do
      mandatesList <- Fixtures.mandates
      mandatesList ^. count @?= 1
  , testCase "Mandate fields" $ do
      [mandate] <- fmap (view embedded) Fixtures.mandates
      mandate ^. status @?= MandateValid
      mandate ^. method @?= Directdebit
      mandate ^. mandateReference @?= Nothing
      mandate ^. signatureDate @?= Just "2018-05-07"
  , testCase "Mandate details" $ do
      [mandate] <- fmap (view embedded) Fixtures.mandates
      -- TODO: figure out why `mandate ^. details` throws an infinite type error
      let Just details = _mandateDetails mandate

      details ^. consumerName @?= Just "John Doe"
      details ^. consumerAccount @?= Just "NL55INGB0000000000"
      details ^. consumerBic @?= Just "INGBNL2A"
  ]

chargebackTests = testGroup "Chargeback tests"
  [ testCase "Chargeback list count" $ do
      chargebackList <- Fixtures.chargebacks
      chargebackList ^. count @?= 1
  , testCase "Chargeback fields" $ do
      [chargeback] <- fmap (view embedded) Fixtures.chargebacks
      -- Resolves to 2018-03-14T17:00:52
      let time = Time.UTCTime (Time.fromGregorian 2018 3 14) (17*60*60 + 0*60 + 52)
      chargeback ^. amount @?= (def & value .~ "43.38" & currency .~ Currency.USD)
      chargeback ^. settlementAmount @?= Just (def & value .~ "35.07")
      chargeback ^. createdAt @?= time
      chargeback ^. reversedAt @?= Nothing
      chargeback ^. paymentId @?= "tr_WDqYK6vllg"
  ]
