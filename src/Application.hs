
module Application where

import qualified System.Directory   as S
import qualified System.Environment as S

import Control.Monad.Reader         (runReaderT)
import Control.Monad.Trans.Except   (ExceptT(..))
import Control.Concurrent.Async     (race_)
import Control.Concurrent           (threadDelay)

import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors -- (cors,simpleCors,simpleCorsResourcePolicy
                                   -- ,corsRequestHeaders)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, modifyMVar)
import Network.Wai.Handler.WebSockets (websocketsOr)

import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr,
                                    pgPoolSize, runSqlPool)
import Data.Yaml.Config            (loadYamlSettings, useEnv)
import Data.Monoid ((<>))

import Servant 
-- import Control.Natural (wrapNT)

import Config   (configSettingsYml)
import Settings (AppSettings(..)
                , Settings(..)
                , makePool
                , setLogger
                , App(..)
                , Environment (..)
                )
import Model    (doMigrations)
import Api      (server, API(..))


getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

startApp :: IO ()
startApp = do
  appSettings <- getAppSettings
  pool <- makePool appSettings
  let port = appPort appSettings
      settings = Settings { getPool = pool
                          , getEnv = Production
                          }
      logger = setLogger settings
  runSqlPool doMigrations pool
  Warp.run port
    $ corsWithContentType
    $ logger
    $ app settings

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        , corsMethods = simpleMethods <> ["PUT", "DELETE"]
        }

apiProxy :: Proxy API
apiProxy = Proxy
        
app :: Settings -> Application
app = serve apiProxy . appToServer

appToServer :: Settings -> Server API
appToServer settings = enter (readerToExcept settings) server

readerToExcept :: Settings -> App :~> ExceptT ServantErr IO
readerToExcept = runReaderTNat

-- | For yesod devel, return the Warp settings and WAI Application.
startDevelApp :: IO ()
startDevelApp = race_ watchTermFile $ do
  appSettings <- getAppSettings
  pool <- makePool appSettings
  let -- port = appPort appSettings
      settings = Settings { getPool = pool
                          , getEnv = Development
                          }
      logger = setLogger settings
  runSqlPool doMigrations pool
  port <- fmap read $ S.getEnv "PORT"
  displayPort <- S.getEnv "DISPLAY_PORT"
  putStrLn $ "Running in development mode on port " ++ show port
  putStrLn $ "But you should connect to port " ++ displayPort
  Warp.run port
    $ corsWithContentType
    $ logger
    $ app settings
  
-- | Would certainly be more efficient to use fsnotify, but this is
-- simpler.
watchTermFile :: IO ()
watchTermFile = loop
  where
    loop = do
      exists <- S.doesFileExist "yesod-devel/devel-terminate"
      if exists
        then return ()
        else do
            threadDelay 100000
            loop
