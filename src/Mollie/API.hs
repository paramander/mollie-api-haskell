{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Mollie.API
    ( chargebackClient
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

data MollieAPI route = MollieAPI
    { customerAPI     :: route :- ToServantApi CustomerAPI
    , chargebackAPI   :: route :- ToServantApi ChargebackAPI
    , methodAPI       :: route :- ToServantApi MethodAPI
    , mandateAPI      :: route :- ToServantApi MandateAPI
    , paymentAPI      :: route :- ToServantApi PaymentAPI
    , refundAPI       :: route :- ToServantApi RefundAPI
    , subscriptionAPI :: route :- ToServantApi SubscriptionAPI
    } deriving Generic

type MollieServantAPI = "v2" :> ToServantApi MollieAPI

servantApi :: Proxy MollieServantAPI
servantApi = Proxy

chargebackClient :: ChargebackAPI (AsClientT ClientM)
chargebackClient = fromServant $ chargebackAPI mollieClient

customerClient :: CustomerAPI (AsClientT ClientM)
customerClient = fromServant $ customerAPI mollieClient

methodClient :: MethodAPI (AsClientT ClientM)
methodClient = fromServant $ methodAPI mollieClient

mandateClient :: MandateAPI (AsClientT ClientM)
mandateClient = fromServant $ mandateAPI mollieClient

paymentClient :: PaymentAPI (AsClientT ClientM)
paymentClient = fromServant $ paymentAPI mollieClient

refundClient :: RefundAPI (AsClientT ClientM)
refundClient = fromServant $ refundAPI mollieClient

subscriptionClient :: SubscriptionAPI (AsClientT ClientM)
subscriptionClient = fromServant $ subscriptionAPI mollieClient

mollieClient :: MollieAPI (AsClientT ClientM)
mollieClient = fromServant $ client servantApi

runMollie :: ClientEnv -> ClientM a -> IO (Either ResponseError a)
runMollie env apiFunction = do
    res <- runClientM apiFunction env
    case res of
        Left failure -> do
            return $ Left $ handleError failure
        Right success ->
            return $ Right success
