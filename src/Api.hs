
module Api where

import Settings (App)
import Servant
import Database.Persist.Sql
import Database.Persist.Class
import GHC.Types
import Model
import Api.User
import Api.Company
import Api.Others


type API = UserAPI
      :<|> CompanyAPI
      :<|> ImageAPI


server :: ServerT API App
server = userServer
    :<|> companyServer
    :<|> imageServer

