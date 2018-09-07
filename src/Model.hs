{-# LANGUAGE ScopedTypeVariables #-}

module Model where

import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader(..))

import Database.Persist.Sql
import Database.Persist.Quasi
import Database.Persist.TH
import Data.Text
import Data.Typeable
import Settings (Settings(..))
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime
import Data.Time.Calendar (Day)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

doMigrations :: MonadIO m => SqlPersistT m ()
doMigrations = runMigration migrateAll


runDB :: (MonadIO m, MonadReader Settings m) => SqlPersistT IO b -> m b
runDB query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

