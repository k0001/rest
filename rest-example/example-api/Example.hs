{-# LANGUAGE OverloadedStrings #-}
module Example (exampleBlog) where

import Control.Applicative

import ApiTypes (ServerData (..))
import qualified Database.PostgreSQL.Simple as PG


-- Set up the server state
exampleBlog :: IO ServerData
exampleBlog = ServerData
  <$> PG.connectPostgreSQL "dbname=blog1 user=zurihac password=zurihac port=5432"
