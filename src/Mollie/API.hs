module Mollie.API
    ( Mollie
    , Env
    , createEnv
    , runMollie
    , Reader.liftIO
    ) where

import qualified Control.Monad.Reader    as Reader
import qualified Data.Text               as Text
import           Mollie.API.Internal
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

{-|
  Create a new Env from a Mollie API key.

  This key should start with either `test_` or `live_`.
-}
createEnv :: Text.Text -> IO Env
createEnv key = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    return Env
        { env_key     = key
        , env_manager = manager
        }

{-|
  Runs handlers with the provided environment against the Mollie API.
-}
runMollie :: Env -> Mollie a -> IO a
runMollie env query = Reader.runReaderT query env
