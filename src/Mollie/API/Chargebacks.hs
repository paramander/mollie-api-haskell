{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Mollie.API.Chargebacks
    (
    -- Re-export relevant types
    Chargeback (..)
    -- Lens getters
    , Mollie.API.Chargebacks.id
    , amount
    , settlementAmount
    , createdAt
    , reversedAt
    , paymentId
    , payment
    ) where

import           Control.Lens        (makeFieldsNoPrefix)
import           Mollie.API.Types

makeFieldsNoPrefix ''Chargeback
