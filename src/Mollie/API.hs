module Mollie.API
    ( Mollie
    , Env
    , createEnv
    , runMollie
    , Reader.liftIO
    ) where

import qualified Control.Monad.Reader        as Reader
import qualified Data.Text                   as Text
import           Mollie.API.Internal
import qualified Network.HTTP.Client         as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import qualified OpenSSL.Session             as OpenSSL
import qualified Paths_mollie_api_haskell    as Self

{-|
  Create a new Env from a Mollie API key.

  This key should start with either `test_` or `live_`.
-}
createEnv :: Text.Text -- ^ key
          -> IO Env
createEnv key = HTTP.withOpenSSL $ do
    sslContext <- OpenSSL.context
    OpenSSL.contextSetVerificationMode sslContext OpenSSL.VerifyPeer
        { OpenSSL.vpFailIfNoPeerCert = True
        , OpenSSL.vpClientOnce       = False
        , OpenSSL.vpCallback         = Nothing
        }
    cacert <- Self.getDataFileName "data/cacert.pem"
    OpenSSL.contextSetCAFile sslContext cacert
    manager <- HTTP.newManager . HTTP.opensslManagerSettings $ return sslContext
    return Env
        { env_key     = key
        , env_manager = manager
        }

{-|
  Runs handlers with the provided environment against the Mollie API.
-}
runMollie :: Env -> Mollie a -> IO a
runMollie env query = Reader.runReaderT query env
