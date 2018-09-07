{-# LANGUAGE DeriveGeneric #-}

module Api.User where

import GHC.Generics
import Data.Aeson.Types

import Control.Monad.Trans.Either

import Settings (App, Settings)
import Servant
import Model
import Database.Persist.Sql
import Api.Crud
import Api.Util
import Data.Text
import Data.Monoid
import Debug.Trace

data Login = Login
  { user :: Text
  , password :: Text
  } deriving (Generic)

instance FromJSON Login

data Token = Token
  { token :: Text
  , payload :: Key Company
  } deriving (Generic)

instance ToJSON Token

type UserAPI
  = "users" :> (CrudAPI User
               :<|> "login" :> ReqBody '[JSON] Login :> Post '[JSON] Token
               )
               

userServer :: ServerT UserAPI App
userServer =
  defaultCrudServer :<|> login

invalidUser = "Invalid user or password"
  
login :: Login -> App Token
login (Login {..}) = do
  eUser <- runDB $ runEitherT $ do
             -- Obtener el usuario
             liftMaybeEitherT
               invalidUser $
               getBy $ UniqueUser user
  case trace ("USER LOGIN ERROR :" <> show eUser) eUser of
    Left e ->
      throwError $ err404 { errBody = e }
    Right (Entity _ dbUser) ->
        if Just password == userPassword dbUser
        then
          return $ Token "token" (userCompanyId dbUser)
        else
          throwError $ err404 { errBody = invalidUser }
