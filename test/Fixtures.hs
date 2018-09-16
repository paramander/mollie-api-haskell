module Fixtures where

import           Data.Aeson               (decode, eitherDecode)
import           Data.Aeson.Types         (FromJSON)
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (fromMaybe)
import qualified Mollie.API.Customers     as Customers
import qualified Mollie.API.Mandates      as Mandates
import qualified Mollie.API.Payments      as Payments
import qualified Mollie.API.Refunds       as Refunds
import qualified Mollie.API.Subscriptions as Subscriptions

payments :: IO (Payments.List Payments.Payment)
payments =
    readJSONFile "test/fixtures/payments.json"

customers :: IO (Customers.List Customers.Customer)
customers =
    readJSONFile "test/fixtures/customers.json"

refunds :: IO (Refunds.List Refunds.Refund)
refunds =
    readJSONFile "test/fixtures/refunds.json"

subscriptions :: IO (Subscriptions.List Subscriptions.Subscription)
subscriptions =
    readJSONFile "test/fixtures/subscriptions.json"

mandates :: IO ( Mandates.List Mandates.Mandate)
mandates =
    readJSONFile "test/fixtures/customers_mandates.json"

readJSONFile :: (FromJSON a) => FilePath -> IO a
readJSONFile path = do
    contents <- BL.readFile path
    case eitherDecode contents of
        Right a  -> return a
        Left err -> error err
