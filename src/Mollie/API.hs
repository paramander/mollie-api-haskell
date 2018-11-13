{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Mollie.API
    ( MollieServantAPI
    , MollieAPI
    , HalJSON
    , chargebackClient
    , customerClient
    , mandateClient
    , methodClient
    , paymentClient
    , refundClient
    , subscriptionClient
    , createEnv
    , runMollie
    ) where

import           Data.Proxy               (Proxy (..))
import           GHC.Generics             (Generic)
import           Mollie.API.Chargebacks   (ChargebackAPI)
import           Mollie.API.Customers     (CustomerAPI)
import           Mollie.API.Internal
import           Mollie.API.Mandates      (MandateAPI)
import           Mollie.API.Methods       (MethodAPI)
import           Mollie.API.Payments      (PaymentAPI)
import           Mollie.API.Refunds       (RefundAPI)
import           Mollie.API.Subscriptions (SubscriptionAPI)
import           Mollie.API.Types         (ResponseError)
import           Servant.API
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic
-- import qualified Paths_mollie_api_haskell    as Self

{-|
  All v2 endpoints of Mollie API.
-}
data MollieAPI route = MollieAPI
    { customerAPI     :: route :- ToServantApi CustomerAPI
    , chargebackAPI   :: route :- ToServantApi ChargebackAPI
    , methodAPI       :: route :- ToServantApi MethodAPI
    , mandateAPI      :: route :- ToServantApi MandateAPI
    , paymentAPI      :: route :- ToServantApi PaymentAPI
    , refundAPI       :: route :- ToServantApi RefundAPI
    , subscriptionAPI :: route :- ToServantApi SubscriptionAPI
    } deriving Generic

{-|
  The fully combined Mollie API definition of Haskell.
-}
type MollieServantAPI = "v2" :> ToServantApi MollieAPI

servantApi :: Proxy MollieServantAPI
servantApi = Proxy

{-|
  Record that holds the endpoints for the Chargeback API.
  Usage:

@
import Mollie.API
import Mollie.API.Chargebacks

env <- createEnv "test_mollieapikeyexample"
let chargebacksResult = runMollie env (getChargebacks chargebackClient)
@
-}
chargebackClient :: ChargebackAPI (AsClientT ClientM)
chargebackClient = fromServant $ chargebackAPI mollieClient

{-|
  Record that holds the endpoints for the Customer API.
  Usage:

@
import Mollie.API
import Mollie.API.Customers

env <- createEnv "test_mollieapikeyexample"
let customersResult = runMollie env (getCustomers customerClient)
@
-}
customerClient :: CustomerAPI (AsClientT ClientM)
customerClient = fromServant $ customerAPI mollieClient

{-|
  Record that holds the endpoints for the Method API.
  Usage:

@
import Mollie.API
import Mollie.API.Methods

env <- createEnv "test_mollieapikeyexample"
let methodsResult = runMollie env (getMethods methodClient)
@
-}
methodClient :: MethodAPI (AsClientT ClientM)
methodClient = fromServant $ methodAPI mollieClient

{-|
  Record that holds the endpoints for the Mandate API.
  Usage:

@
import Mollie.API
import Mollie.API.Mandates

env <- createEnv "test_mollieapikeyexample"
let mandatesResult = runMollie env ((getCustomerMandates mandateClient) "cst_eaaEuAnqW")
@
-}
mandateClient :: MandateAPI (AsClientT ClientM)
mandateClient = fromServant $ mandateAPI mollieClient

{-|
  Record that holds the endpoints for the Payments API.
  Usage:

@
import Mollie.API
import Mollie.API.Payments

env <- createEnv "test_mollieapikeyexample"
let paymentsResult = runMollie env (getPayments paymentClient)
@
-}
paymentClient :: PaymentAPI (AsClientT ClientM)
paymentClient = fromServant $ paymentAPI mollieClient

{-|
  Record that holds the endpoints for the Refunds API.
  Usage:

@
import Mollie.API
import Mollie.API.Refunds

env <- createEnv "test_mollieapikeyexample"
let refundsResult = runMollie env (getRefunds refundClient)
@
-}
refundClient :: RefundAPI (AsClientT ClientM)
refundClient = fromServant $ refundAPI mollieClient

{-|
  Record that holds the endpoints for the Subscriptions API.
  Usage:

@
import Mollie.API
import Mollie.API.Subscriptions

env <- createEnv "test_mollieapikeyexample"
let subscriptionsResult = runMollie env ((getCustomerSubscriptions refundClient) "cst_eaaEuAnqW")
@
-}
subscriptionClient :: SubscriptionAPI (AsClientT ClientM)
subscriptionClient = fromServant $ subscriptionAPI mollieClient

mollieClient :: MollieAPI (AsClientT ClientM)
mollieClient = fromServant $ client servantApi

{-|
  Execute an API call to the Mollie API. Uses Servant under the hood.
-}
runMollie :: ClientEnv -> ClientM a -> IO (Either ResponseError a)
runMollie env apiFunction = do
    res <- runClientM apiFunction env
    case res of
        Left failure -> do
            return $ Left $ handleError failure
        Right success ->
            return $ Right success
