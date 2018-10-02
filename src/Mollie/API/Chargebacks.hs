{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Chargebacks
    (
    Chargeback (..)
    -- Lens getters
    , Mollie.API.Chargebacks.id
    , amount
    , settlementAmount
    , createdAt
    , reversedAt
    , paymentId
    ) where

import           Control.Lens     (makeFieldsNoPrefix)
import qualified Data.Text        as Text
import qualified Data.Time        as Time
import           Mollie.API.Types

data Chargeback = Chargeback
    { _id               :: Text.Text
    -- ^Mollies reference to the chargeback.
    , _amount           :: Amount
    -- ^The amount charged back by the consumer.
    , _settlementAmount :: Amount
    -- ^The amount that will be deducted from your account
    , _createdAt        :: Time.UTCTime
    -- ^The date and time the chargeback was issued.
    , _reversedAt       :: Maybe Time.UTCTime
    -- ^The date and time the chargeback was issued.
    , _paymentId        :: Text.Text
    -- ^The unique identifier of the payment this chargeback was issued for.
    }

makeFieldsNoPrefix ''Chargeback
