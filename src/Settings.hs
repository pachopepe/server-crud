{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# Language CPP #-}

-- Taked or based on
-- https://github.com/parsonsmatt/servant-persistent/blob/master/src/Config.hs

module Settings where

import Control.Monad.Except                 (ExceptT)
import Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid                          ((<>))

import Database.Persist.Sql
import Database.Persist.Postgresql          (ConnectionPool,
                                              ConnectionString,
                                              createPostgresqlPool,
                                              PostgresConf(..))
import Network.Wai                          (Middleware)
import Network.Wai.Handler.Warp             (HostPreference)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant                              (ServantErr)
import System.Environment                   (lookupEnv)

import Data.String                 (fromString)
import Data.Aeson                  (FromJSON(..),Result (..), fromJSON, withObject, (.!=),
                                    (.:?), (.:), Value(..))
import Data.FileEmbed              (embedFile)

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

data AppSettings = AppSettings
  { appDatabaseConf :: PostgresConf
  , appHost         :: HostPreference
  , appPort         :: Int
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
        appDatabaseConf           <- o .: "database"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        return AppSettings {..}

environment :: a -> a -> a -> Environment -> a
environment dev _test _prod Development = dev
environment _dev test _prod Test = test
environment _dev _test prod Production = prod

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
{-
newtype App a
    = App
    { runApp :: ReaderT Settings (ExceptT ServantErr IO) a
    } deriving ( Functor, Applicative, Monad, MonadReader Settings,
                 MonadIO)
-}
type App = ReaderT Settings (ExceptT ServantErr IO)


-- | The Settings for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Settings
    = Settings
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Settings -> Middleware
setLogger = environment logStdoutDev id logStdout . getEnv

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: AppSettings -> IO ConnectionPool
makePool appSettings = mkPool'
  where
    mkPool' = runStdoutLoggingT $ createPostgresqlPool
              (pgConnStr  $ appDatabaseConf appSettings)
              (pgPoolSize $ appDatabaseConf appSettings)
