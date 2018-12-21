module Fixtures where

import           Data.Aeson               (decode, eitherDecode)
import           Data.Aeson.Types         (FromJSON)
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (fromMaybe)
import           Mollie.API.Types

payments :: IO (List Payment)
payments =
    readJSONFile "test/fixtures/payments.json"

customers :: IO (List Customer)
customers =
    readJSONFile "test/fixtures/customers.json"

refunds :: IO (List Refund)
refunds =
    readJSONFile "test/fixtures/refunds.json"

subscriptions :: IO (List Subscription)
subscriptions =
    readJSONFile "test/fixtures/subscriptions.json"

mandates :: IO (List Mandate)
mandates =
    readJSONFile "test/fixtures/customers_mandates.json"

chargebacks :: IO (List Chargeback)
chargebacks =
    readJSONFile "test/fixtures/chargebacks.json"

readJSONFile :: (FromJSON a) => FilePath -> IO a
readJSONFile path = do
    contents <- BL.readFile path
    case eitherDecode contents of
        Right a  -> return a
        Left err -> error err
