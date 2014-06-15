{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    Arrows
  , ScopedTypeVariables
  #-}
module Db where

import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Data.Text (Text)
import Data.Time
import System.Locale
import qualified Data.Text as T

import Database.HaskellDB.PrimQuery (Literal (..))
import Database.HaskellDB.Query (ShowConstant (..))
import Karamaan.Opaleye.ExprArr (constant)
import Karamaan.Opaleye.Manipulation (executeInsertConnDef)
import Karamaan.Opaleye.RunQuery as RQ
import Karamaan.Opaleye.Table
import Karamaan.Opaleye.Wire (Wire (Wire))
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Karamaan.Opaleye.ExprArr               as ExprArr

import ApiTypes
import Type.Post (Post (Post))
import qualified Type.Post as Post
import qualified Type.User as User

instance ShowConstant UTCTime where
  showConstant = StringLit . formatTime defaultTimeLocale format
    where
      format :: String
      format = "%Y-%m-%dT%H:%M:%SZ"

instance ShowConstant Text where
  showConstant = StringLit . T.unpack

postTable :: Table (Wire Int, Wire Text, Wire UTCTime, Wire Post.Title, Wire Text)
postTable = Table "posts" (Wire "id", Wire "author", Wire "created_time", Wire "title", Wire "content")

fromTable :: (Int, User.Name, UTCTime, Post.Title, Text) -> Post
fromTable (a,b,c,d,e) = Post a b c d e

selectAllPosts :: BlogApi [Post]
selectAllPosts = do
  pg <- asks pgConn
  fmap (fmap fromTable) . liftIO . RQ.runQueryDefault (queryTable postTable) $ pg

createPost :: Post -> BlogApi ()
createPost post = do
  pg <- asks pgConn
  liftIO $ PG.withTransaction pg $ do
    let insertExpr :: ExprArr.Expr (Maybe (Wire Int), Maybe (Wire Text), Maybe (Wire UTCTime), Maybe (Wire Post.Title), Maybe (Wire Text))
        insertExpr = proc () -> do
          author <- constant (Post.author post) -< ()
          time <- constant (Post.createdTime post) -< ()
          title <- constant (Post.title post) -< ()
          content <- constant (Post.content post) -< ()
          returnA -< (Nothing, Just author, Just time, Just title, Just content)
    void $ executeInsertConnDef pg postTable insertExpr
