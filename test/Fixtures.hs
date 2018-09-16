module Fixtures where

import           Data.Aeson                       (decode, eitherDecode)
import           Data.Aeson.Types                 (FromJSON)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe                       (fromMaybe)
import qualified Mollie.API.Payments  as Payments

payments :: IO (Payments.List Payments.Payment)
payments =
    readJSONFile "test/fixtures/payments.json"

readJSONFile :: (FromJSON a) => FilePath -> IO a
readJSONFile path = do
    contents <- BL.readFile path
    case eitherDecode contents of
        Right a -> return a
        Left err -> error err
