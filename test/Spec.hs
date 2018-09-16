import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import qualified Fixtures
import qualified Mollie.API.Payments as Payments

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,3] @?= EQ

  , testCase "Payment list count" $ do
      paymentsList <- Fixtures.payments
      Payments.list_count paymentsList `compare` 1 @?= EQ
  ]
