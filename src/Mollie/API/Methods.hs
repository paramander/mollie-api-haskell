{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Mollie.API.Methods
    ( getMethod
    , getMethods
    , MethodAPI
    ) where

import qualified Control.Lens        as Lens
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.TH       as Aeson
import qualified Data.Aeson.Types    as Aeson
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Mollie.API.Helpers
import           Mollie.API.Internal (HalJSON)
import           Mollie.API.Types
import           Servant.API
import           Servant.API.Generic

data MethodAPI route = MethodAPI
    { getMethods :: route :- "methods"
                    :> Get '[HalJSON] (List Method)
    -- ^Handler to get a list of payment methods. See https://docs.mollie.com/reference/v2/methods-api/list-methods
    , getMethod  :: route :- "methods"
                    :> Capture "id" PaymentMethod
                    :> Get '[HalJSON] Method
    -- ^Handler to get a payment method by its identifier. See https://docs.mollie.com/reference/v2/methods-api/get-method
    } deriving Generic
