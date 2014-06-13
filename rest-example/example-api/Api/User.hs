{-# LANGUAGE
    Arrows
  , ScopedTypeVariables
  #-}
module Api.User (resource) where

import Control.Monad.Reader
import qualified Data.Foldable as F
import qualified Data.List     as List
import qualified Data.Text     as T

import Rest
import qualified Rest.Resource as R

import ApiTypes
import Db
import Type.User (User)
import Type.UserInfo (UserInfo (..))
import Type.UserSignupError (UserSignupError (..))
import qualified Type.User     as User
import qualified Type.UserInfo as UserInfo

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
  usrs <- liftApi selectAllUsers
  return . map toUserInfo . take (count r) . drop (offset r) . List.sort $ usrs

-- | Convert a User into a representation that is safe to show to the public.
toUserInfo :: User -> UserInfo
toUserInfo u = UserInfo { UserInfo.name = User.name u }

create :: Handler BlogApi
create = mkInputHandler ({-xmlJsonE . -} xmlJsonO . xmlJsonI) $ \usr -> do
  liftApi $ createUser usr
  return $ toUserInfo usr

validPassword :: User.User -> Bool
validPassword = (> 1) . T.length . User.password

validUserName :: User -> [User] -> Bool
validUserName u usrs =
  let un        = User.name u
      available = F.all ((un /=). User.name) usrs
      nonEmpty  = (> 1) . T.length $ un
  in available && nonEmpty
