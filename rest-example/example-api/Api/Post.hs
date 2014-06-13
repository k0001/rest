{-# LANGUAGE Arrows, FlexibleContexts, ScopedTypeVariables #-}
module Api.Post (resource) where

import Control.Monad.Error (ErrorT, throwError)
import Control.Monad.Reader
import Data.List (sortBy, find)
import Data.Ord (comparing)
import System.Locale
import Data.Time
import qualified Data.Foldable as F
import qualified Data.Text     as T
import Data.Text (Text)

import Rest (Handler, ListHandler, Range (..), Reason (..), Resource, Void, domainReason, mkInputHandler, mkIdHandler, mkListing, mkResourceReader, named, singleRead,
             withListing, xmlJson, xmlJsonE, xmlJsonO)
import qualified Rest.Resource as R

import ApiTypes
import Type.CreatePost (CreatePost)
import Type.Post (Post (Post))
import Type.PostError (PostError (..))
import Type.User (User)
import Type.UserPost (UserPost (UserPost))
import qualified Type.CreatePost as CreatePost
import qualified Type.Post       as Post
import qualified Type.User       as User

import Control.Arrow
import Database.HaskellDB.Query (ShowConstant(..))
import Database.HaskellDB.PrimQuery (Literal (..))
import Karamaan.Opaleye.Manipulation (executeInsertConnDef)
import Karamaan.Opaleye.RunQuery as RQ
import Karamaan.Opaleye.Table
import Karamaan.Opaleye.Wire (Wire(Wire))
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Karamaan.Opaleye.ExprArr as ExprArr
import Karamaan.Opaleye.ExprArr (constant)

-- | Post extends the root of the API with a reader containing the ways to identify a Post in our URLs.
-- Currently only by the title of the post.
type WithPost = ReaderT Int BlogApi

-- | Defines the /post api end-point.
resource :: Resource BlogApi WithPost Int () Void
resource = mkResourceReader
  { R.name   = "post" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("id", singleRead id)]
  , R.list   = const list -- list is requested by GET /post which gives a listing of posts.
  , R.create = Just create -- PUT /post to create a new Post.
  , R.get    = Just get
  }

postTable :: Table (Wire Int, Wire Text, Wire UTCTime, Wire Post.Title, Wire Text)
postTable = Table "posts" (Wire "id", Wire "author", Wire "created_time", Wire "title", Wire "content")

fromTable (a,b,c,d,e) = Post a b c d e

get :: Handler WithPost
get = mkIdHandler xmlJsonO $ \_ ident -> do
  pgC <- lift . lift . asks $ pgConn
  psts <- fmap (fmap fromTable) . liftIO . RQ.runQueryDefault (queryTable postTable) $ pgC
  maybe (throwError NotFound) return . find (\p -> Post.id p == ident) $ psts

-- | List Posts with the most recent posts first.
list :: ListHandler BlogApi
list = mkListing xmlJsonO $ \r -> do
  pgC <- asks pgConn
  psts <- fmap (fmap fromTable) . liftIO . RQ.runQueryDefault (queryTable postTable) $ pgC
  return . take (count r) . drop (offset r) . sortBy (flip $ comparing Post.createdTime) $ psts

instance ShowConstant UTCTime where
  showConstant = StringLit . formatTime defaultTimeLocale format
    where
      format :: String
      format = "%Y-%m-%dT%H:%M:%SZ"
instance ShowConstant Text where
  showConstant = StringLit . T.unpack


create :: Handler BlogApi
create = mkInputHandler (xmlJsonE . xmlJson) $ \(UserPost usr pst) -> do
  pgC <- asks pgConn
  -- Make sure the credentials are valid
  checkLogin usr
  psts <- fmap (fmap fromTable) . liftIO . RQ.runQueryDefault (queryTable postTable) $ pgC
  post <- liftIO $ toPost usr pst
  -- Validate and save the post in the same transaction.
  merr <- do
    let vt = validTitle pst psts
    if not vt
      then return . Just $ domainReason (const 400) InvalidTitle
      else if not (validContent pst)
        then return . Just $ domainReason (const 400) InvalidContent
        else liftIO $ PG.withTransaction pgC $ do
          let insertExpr :: ExprArr.Expr (Maybe (Wire Int), Maybe (Wire Text), Maybe (Wire UTCTime), Maybe (Wire Post.Title), Maybe (Wire Text))
              insertExpr = proc () -> do
                author <- constant (Post.author post) -< ()
                time <- constant (Post.createdTime post) -< ()
                title <- constant (Post.title post) -< ()
                content <- constant (Post.content post) -< ()
                returnA -< (Nothing, Just author, Just time, Just title, Just content)
          executeInsertConnDef pgC postTable insertExpr
          return Nothing
  maybe (return post { Post.id = (-1) } ) throwError merr

-- | Convert a User and CreatePost into a Post that can be saved.
toPost :: User -> CreatePost -> IO Post
toPost u p = do
  t <- getCurrentTime
  return Post
    { Post.id          = undefined
    , Post.author      = User.name u
    , Post.createdTime = t
    , Post.title       = CreatePost.title p
    , Post.content     = CreatePost.content p
    }

-- | A Post's title must be unique and non-empty.
validTitle :: CreatePost -> [Post] -> Bool
validTitle p psts =
  let pt        = CreatePost.title p
      nonEmpty  = (>= 1) . T.length $ pt
      available = F.all ((pt /=) . Post.title) psts
  in available && nonEmpty

-- | A Post's content must be non-empty.
validContent :: CreatePost -> Bool
validContent = (>= 1) . T.length . CreatePost.content

-- | Throw an error if the user isn't logged in.
checkLogin :: User -> ErrorT (Reason e) BlogApi ()
checkLogin usr = do
  return ()
--  usrs <- liftIO . atomically . readTVar =<< asks users
--  unless (usr `F.elem` usrs) $ throwError NotAllowed
