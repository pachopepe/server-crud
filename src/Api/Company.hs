{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Api.Company where

import GHC.Generics
import Data.Aeson.Types
import Settings (App)
import Servant
import Model
import Database.Persist.Sql
import Api.Crud
import Api.Util
import Data.Text (Text)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime
import Data.Time.Calendar (Day)


type CompanyAPI = "companies" :> CrudAPI Company

companyServer :: ServerT CompanyAPI App
companyServer = defaultCrudServer
