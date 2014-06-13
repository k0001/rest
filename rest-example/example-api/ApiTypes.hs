{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module ApiTypes where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Database.PostgreSQL.Simple as PG

data ServerData = ServerData
  { pgConn :: PG.Connection
  }

newtype BlogApi a = BlogApi { unBlogApi :: ReaderT ServerData IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader ServerData
           )

runBlogApi :: ServerData -> BlogApi a -> IO a
runBlogApi serverdata = flip runReaderT serverdata . unBlogApi

class Monad m => LiftApi m where
  liftApi :: BlogApi a -> m a

instance LiftApi BlogApi where
  liftApi = id

instance LiftApi m => LiftApi (ReaderT a m) where
  liftApi = lift . liftApi

instance (Error e, LiftApi m) => LiftApi (ErrorT e m) where
  liftApi = lift . liftApi
