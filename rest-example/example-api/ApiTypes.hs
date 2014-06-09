{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ApiTypes where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO)
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
