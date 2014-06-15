{-# LANGUAGE
    Arrows
  , ScopedTypeVariables
  #-}
module Api.User (resource) where

import Control.Monad (unless)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans (liftIO)
import qualified Data.List     as List
import qualified Data.Text     as T

import Rest
import qualified Rest.Resource as R

import ApiTypes
import Db ()
import Type.User (User)
import Type.UserInfo (UserInfo (..))
import Type.UserSignupError (UserSignupError (..))
import qualified Type.User     as User
import qualified Type.UserInfo as UserInfo


import Prelude hiding (sum)
import qualified Karamaan.Opaleye.Predicates as P
import Database.PostgreSQL.Simple.Transaction as PG
import Karamaan.Opaleye.Table (Table(Table), queryTable)
import qualified Karamaan.Opaleye.ExprArr as ExprArr
import qualified Karamaan.Opaleye.Operators2 as Op2
import Karamaan.Opaleye.Manipulation (executeInsertConnDef)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Control.Category ((<<<))
import Control.Arrow (returnA)

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
  pgConn' <- asks pgConn
  let toUser (name', password') = User.User name' password'
  usrs <- fmap (fmap toUser) $ liftIO $ RQ.runQueryDefault (queryTable usersTable) pgConn'
  return . map toUserInfo . take (count r) . drop (offset r) . List.sort $ usrs


usersTable :: Table (Wire T.Text, Wire T.Text)
usersTable = Table "users" (Wire "name", Wire "password")

-- | Convert a User into a representation that is safe to show to the public.
toUserInfo :: User -> UserInfo
toUserInfo u = UserInfo { UserInfo.name = User.name u }

create :: Handler BlogApi
create = mkInputHandler (xmlJsonE . xmlJsonO . xmlJsonI) $ \usr -> do
  unless (validPassword usr) $ do
     throwError $ domainReason (const 400) InvalidPassword
  pgConn' <- asks pgConn
  _ <- liftIO $ PG.withTransaction pgConn' $ do
      -- check whether the same user name already exists
      results <- flip RQ.runQueryDefault pgConn' $ proc () -> do
        usrs <- queryTable usersTable -< ()
        name' <- Op2.constant (User.name usr) -< ()
        P.restrict <<< Op2.eq -< (name', fst usrs)
        returnA -< ()
      print results
      if not (null (results :: [()])) -- this is not the right type
         then return $ Left $ domainReason (const 400) InvalidUserName
         else do
            -- insert the new user
            _ <- executeInsertConnDef pgConn' usersTable $ proc () -> do
              name' <- ExprArr.constant (User.name usr) -< ()
              password' <- ExprArr.constant (User.password usr) -< ()
              returnA -< (Just name', Just password')
            return $ Right ()
  return $ toUserInfo usr

validPassword :: User.User -> Bool
validPassword = (> 1) . T.length . User.password
