
module Api.Crud where

import Settings (App,Settings)
import Servant
import Model
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

type Page = Int
type PageSize = Int

type CrudAPI entity
  = QueryParam "page" Page :> QueryParam "pageSize" PageSize :> Get '[JSON] [Entity entity]
    :<|> Capture "key" (Key entity) :> Get '[JSON] (Maybe entity)
    :<|> ReqBody '[JSON] entity :> Post '[JSON] (Entity entity)
    :<|> Capture "key" (Key entity) :> ReqBody '[JSON] entity :> Put '[JSON] (Maybe entity)
    :<|> Capture "key" (Key entity) :> Delete '[JSON] ()


data CRUD entity = CRUD
  { retrieveAll   :: Maybe Page -> Maybe PageSize -> SqlPersistT IO [Entity entity]
  , retrieveByKey :: Key entity -> SqlPersistT IO (Maybe entity)
  , createEntity  :: entity -> SqlPersistT IO (Entity entity)
  , replaceEntity :: Key entity -> entity -> SqlPersistT IO ()
  , deleteEntity  :: Key entity -> SqlPersistT IO ()
  }

defaultPageSize :: Int
defaultPageSize = 25

options :: Int -> Int -> [SelectOpt record]
options page pageSize = [ OffsetBy $ (page - 1) * pageSize, LimitTo pageSize ]

mOptions :: Maybe Int -> Maybe Int -> [SelectOpt record]
mOptions Nothing Nothing = []
mOptions (Just page) Nothing = options page defaultPageSize
mOptions Nothing (Just pageSize) = options 1 pageSize
mOptions (Just page) (Just pageSize) = options page pageSize

defaultCrud :: (PersistEntity entity, ToBackendKey SqlBackend entity) => CRUD entity
defaultCrud = CRUD
  { retrieveAll    = retrieveAll'
  , retrieveByKey  = get
  , createEntity   = insertEntity
  , replaceEntity  = replace
  , deleteEntity   = delete
  }
  where retrieveAll' mPage mSize = selectList [] $ mOptions mPage mSize

                                   
defaultCrudServer :: (PersistEntity entity, ToBackendKey SqlBackend entity) => ServerT (CrudAPI entity) App 
defaultCrudServer = crudServer defaultCrud


crudServer :: (MonadIO m, MonadReader Settings m, PersistEntity entity, ToBackendKey SqlBackend entity) =>
              CRUD entity ->
              (Maybe Page -> Maybe PageSize -> m [Entity entity])
              :<|> ((Key entity -> m (Maybe entity))
                    :<|> ((entity -> m (Entity entity))
                           :<|> ((Key entity -> entity -> m (Maybe entity))
                                  :<|> (Key entity -> m ()))))
              -- ServerT (CrudAPI entity) App
crudServer (CRUD {..}) =
                  (\mPage mPageSize -> runDB $ retrieveAll mPage mPageSize)
             :<|> runDB . retrieveByKey
             :<|> runDB . createEntity
             :<|> (\ key entity -> runDB $ do { replaceEntity key entity ; get key })
             :<|> runDB . deleteEntity
