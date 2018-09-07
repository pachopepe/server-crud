
module Api.Others where

import Settings (App)
import Servant
import Model
import Database.Persist.Sql
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (FilePath)
import System.Directory (doesFileExist)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL
import Api.Util
import Api.Crud


type ImageAPI = "images" :>
  ( CrudAPI Image
    :<|> Capture "key" (Key Image) :> "contents" :> Get '[JSON] T.Text
  )

imageServer :: ServerT ImageAPI App
imageServer = defaultCrudServer :<|> getImageAsText

getImageAsText imageId = do
  Image {..} <- runDBE "Image 'id' not found" $
             get imageId
  let fp = getStaticImagePath imageFilePath
  exists <- liftIO $ doesFileExist fp
  if exists
    then liftIO $ T.readFile fp
    else throwError (err404
                     { errBody =
                         "File: '" <> BL.pack fp <> "' not found"
                     })          
