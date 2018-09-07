
module Api.Util where

import Settings (Settings)
import Control.Monad.Reader.Class (MonadReader) 
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error.Class (throwError,MonadError)
import Data.ByteString.Lazy.Internal (ByteString)
import Control.Monad.Trans.Either
import Database.Persist.Sql
import Model (runDB)

import Servant


getEither :: (Monad m) => m (Maybe a) -> err -> m (Either err a)
getEither action err = do
  me <- action
  case me of
    Just e -> return $ Right e
    Nothing -> return $ Left err

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

liftMaybeEitherT :: Monad m => e -> m (Maybe a) -> EitherT e m a
liftMaybeEitherT err action = do
  mResult <- lift action
  case mResult of
    Nothing -> EitherT . return $ Left err
    Just r  -> EitherT . return $ Right r

returnE :: MonadError ServantErr m =>
                 Either ByteString a -> m a
returnE (Right r) = return r
returnE (Left err) =
  throwError $ err404 { errBody = err }

runDBE msg cmd = do
  mrE <- runDB . runEitherT $ liftMaybeEitherT msg $ cmd
  returnE mrE
  
getStaticImagePath :: FilePath -> FilePath
getStaticImagePath filePath = "static/images/"++filePath

