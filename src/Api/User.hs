
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User where

import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import qualified Control.Monad.Metrics       as Metrics
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), insertUnique,
                                              selectFirst, selectList, (==.))
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (AppT (..))
import           Control.Monad.Metrics       (increment, metricsCounters)
import           Data.IORef                  (readIORef)
import           Data.HashMap.Lazy           (HashMap)
import           Data.Text                   (Text)
import           Lens.Micro                  ((^.))
import           Models                      (User (User), runDb, userEmail,
                                              userPassword)
import qualified Models                      as Md
import qualified System.Metrics.Counter      as Counter

type UserAPI =
         "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "name" Text :> Get '[JSON] (User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (User)
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
    maybeUser <- runDb (selectFirst [Md.DUserName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404 { errBody = "Eye" }
         Just person ->
            return person

userByUid :: MonadIO m => Md.Key User -> AppT m (Maybe (Entity User))
userByUid uid = do
    maybeUser <- runDb (selectFirst [Md.DUserId ==. uid] [])
    return maybeUser

-- | Creates a user in the database.
createUser :: MonadIO m => User -> AppT m (Entity User)
createUser p = do
    increment "UserRegistered"
    logDebugNS "web" "creating a user"
    maybeUserId <- runDb (insertUnique (User (userName p) (userEmail p) (userPassword p)))
    case maybeUserId of
      Nothing ->
        throwError err400
      Just uid -> do
        maybeUser <- userByUid uid
        case maybeUser of
          Nothing ->
            throwError err400
          Just user ->
            return user

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
    increment "metrics"
    logDebugNS "web" "metrics"
    metr <- Metrics.getMetrics
    liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)


-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"
