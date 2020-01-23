
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User where

import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import qualified Control.Monad.Metrics       as Metrics
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Servant

import           Config                      (AppT (..))
import           Control.Monad.Metrics       (increment, metricsCounters)
import           Data.IORef                  (readIORef)
import           Data.HashMap.Lazy           (HashMap)
import           Data.Text                   (Text)
import           Lens.Micro                  ((^.))
import           Models                      (User (User), runDb, userEmail,
                                              userName)
import qualified Models                      as Md
import qualified System.Metrics.Counter      as Counter

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" Text :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (Entity User)
    :<|> "metrics" :> Get '[JSON] (HashMap Text Int64)

userApi :: Proxy UserAPI
userApi = Proxy

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser :<|> waiMetrics

-- | Returns all users in the database.
allUsers :: MonadIO m => AppT m [Entity User]
allUsers = do
    increment "allUsers"
    logDebugNS "web" "allUsers"
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: MonadIO m => Text -> AppT m (Entity User)
singleUser str = do
    increment "singleUser"
    logDebugNS "web" "singleUser"
    maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | FIXME: Jakub is stupid because he doesn't know how to do that
-- | Remove me and handle somehow insert error in createUser
idUser :: MonadIO m => Md.Key User -> AppT m (Maybe (Entity User))
idUser uid = do
    maybeUser <- runDb (selectFirst [Md.UserId ==. uid] [])
    return maybeUser

-- | Creates a user in the database.
createUser :: MonadIO m => User -> AppT m (Entity User)
createUser p = do
    increment "createUser"
    logDebugNS "web" "creating a user"
    newUserId <- runDb (insert (User (userName p) (userEmail p)))
    maybeUser <- idUser newUserId
    case maybeUser of
      Nothing ->
        throwError err400
      Just person ->
        return person

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
    increment "metrics"
    logDebugNS "web" "metrics"
    metr <- Metrics.getMetrics
    liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)
