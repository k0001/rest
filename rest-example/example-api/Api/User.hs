{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.User (resource) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically, modifyTVar, readTVar)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans (liftIO)
import qualified Data.List     as List
import Data.Set (Set)
import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T
import qualified Database.PostgreSQL.Simple.Transaction as PG

import Rest (Handler, ListHandler, Range (count, offset), Resource, Void, domainReason, mkInputHandler, mkListing, mkResourceReader, named, singleRead,
             withListing, xmlJsonE, xmlJsonI, xmlJsonO)
import qualified Rest.Resource as R

import ApiTypes (BlogApi, ServerData (..))
import Type.User (User)
import Type.UserInfo (UserInfo (..))
import Type.UserSignupError (UserSignupError (..))
import qualified Type.User     as User
import qualified Type.UserInfo as UserInfo


import Prelude hiding (sum)
import Database.HaskellDB.Query (ShowConstant(..))
import Karamaan.Opaleye.Unpackspec (Unpackspec)
import Karamaan.Opaleye.Table (Table(Table), makeTableDef, queryTable)
import Karamaan.Opaleye.QueryArr (Query, QueryArr)
import qualified Karamaan.Opaleye.ExprArr as ExprArr
import Karamaan.Opaleye.Nullable (Nullable)
import qualified Karamaan.Opaleye.Operators2 as Op2
import qualified Karamaan.Opaleye.Predicates as P
import Karamaan.Opaleye.Manipulation (executeInsertConnDef)
import qualified Karamaan.Opaleye.Operators.Numeric as N
import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.SQL (showSqlForPostgresDefault)
import Control.Category ((<<<))
import Control.Arrow (arr, (&&&), returnA)
import Data.Time.Calendar (Day)

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product (PPOfContravariant, ProductProfunctor, p2, p5)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default, def)
import qualified Database.PostgreSQL.Simple as SQL
import Karamaan.Opaleye.RunQuery as RQ


-- | User extends the root of the API with a reader containing the ways to identify a user in our URLs.
-- Currently only by the user name.
type WithUser = ReaderT User.Name BlogApi

-- | Defines the /user api end-point.
resource :: Resource BlogApi WithUser User.Name () Void
resource = mkResourceReader
  { R.name   = "user" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("name", singleRead id)]
  , R.list   = const list -- requested by GET /user, gives a paginated listing of users.
  , R.create = Just create -- PUT /user creates a new user
  }

list :: ListHandler BlogApi
list = mkListing xmlJsonO $ \r -> do
  pgConn <- asks pgConn
  let toUser (name, password) = User.User name password
  usrs <- fmap (fmap toUser) $ liftIO $ RQ.runQueryDefault (queryTable usersTable) pgConn
  return . map toUserInfo . take (count r) . drop (offset r) . List.sort $ usrs


usersTable :: Table (Wire T.Text, Wire T.Text)
usersTable = Table "users" (Wire "name", Wire "password")


-- | Convert a User into a representation that is safe to show to the public.
toUserInfo :: User -> UserInfo
toUserInfo u = UserInfo { UserInfo.name = User.name u }

instance ShowConstant T.Text where
  showConstant = showConstant . T.unpack


create :: Handler BlogApi
create = mkInputHandler ({-xmlJsonE . -} xmlJsonO . xmlJsonI) $ \usr -> do
  pgConn <- asks pgConn
  _<- liftIO $ PG.withTransaction pgConn $ do
     let insertExpr :: ExprArr.Expr (Maybe (Wire User.Name), Maybe (Wire User.Name))
         insertExpr = proc () -> do
            name <- ExprArr.constant (User.name usr) -< ()
            password <- ExprArr.constant (User.password usr) -< ()
            returnA -< (Just name, Just password)
     executeInsertConnDef pgConn usersTable insertExpr
  return $ toUserInfo usr

  -- usrs <- undefined -- asks users
  -- merr <- liftIO . atomically $ do
  --   vu <- validUserName usr <$> readTVar usrs
  --   if not (validPassword usr)
  --     then return . Just $ domainReason (const 400) InvalidPassword
  --     else if not vu
  --       then return . Just $ domainReason (const 400) InvalidUserName
  --       else modifyTVar usrs (Set.insert usr) >> return Nothing
  -- maybe (return $ toUserInfo usr) throwError merr


validPassword :: User.User -> Bool
validPassword = (> 1) . T.length . User.password

validUserName :: User -> Set User -> Bool
validUserName u usrs =
  let un        = User.name u
      available = F.all ((un /=). User.name) usrs
      nonEmpty  = (> 1) . T.length $ un
  in available && nonEmpty
