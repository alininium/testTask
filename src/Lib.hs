module Lib
  ( server,
    ServerConfig (..)
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Int
import GHC.Generics (Generic)
import qualified Hasql.Connection as Connection
import Hasql.Pool
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Typeable
import Servant
import Types
import Spelling
import DB
import qualified API

import Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import API (toResponse)



api :: Proxy API
api = Proxy

context :: Proxy '[BasicAuthCheck User]
context = Proxy

authCheck :: Pool -> BasicAuthCheck User
authCheck p =
  let check (BasicAuthData username password) = do
        user_creds <- runExceptT . authPoolToExcept p $ getUserPassword (decodeUtf8 username)
        case user_creds of
          Right (u_id, u_password) ->
            if u_password == decodeUtf8 password
              then return (Authorized (User u_id (decodeUtf8 username)))
              else return BadPassword
          Left _ -> return NoSuchUser
  in BasicAuthCheck check


basicAuthServerContext :: Pool -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext p = authCheck p :. EmptyContext

type API = API.GetAllUsersAPI
      :<|> API.GetAllPhrasesAPI
      :<|> API.GetPhrasesFromAPI
      :<|> API.GetPhrasesUnapprovedAPI
      :<|> API.UpdateApprovedAPI
      :<|> API.PostPhraseAPI



data ServerConfig = ServerConfig {
  pool_size    :: Int,
  server_port  :: Int,
  db_port      :: Int,
  db_address   :: BS.ByteString,
  db_password  :: BS.ByteString,
  db_name      :: BS.ByteString,
  db_user      :: BS.ByteString
}

approveIfRightUser :: (MonadDB m, MonadError ServerError m) => Int32 -> Maybe Bool -> User -> m ()
approveIfRightUser u_id force user = do
  root_user_id <- getRootAuthor u_id
  if root_user_id == user_id user
    then updateApproved u_id force
    else throwError err403 {errBody = LBS.fromStrict "You have no access to approve this message"}

app :: Pool -> Application
app pool = serveWithContext api
                            (basicAuthServerContext pool) $
                            hoistServerWithContext api context appMToHandler $
                                                          const                 (toResponse <$> getUsers)
                                                     :<|> const                 (toResponse <$> getPhrases)
                                                     :<|> (\i _               -> toResponse <$> getPhrasesFromUser i)
                                                     :<|> const                 (toResponse <$> getUnapprovedPhrases)
                                                     :<|> (\i approved user   -> toResponse <$> approveIfRightUser i approved user)
                                                     :<|> (\phrase force user -> toResponse <$> (checkSpelling phrase force >> postPhrase user phrase))


  where
    appMToHandler :: AppM a -> Handler a
    appMToHandler m = runReaderT (runAppM m) pool


server :: ServerConfig -> IO ()
server config = do
  pool <- acquire (pool_size config) Nothing conn_settings
  run 8080 $ app pool
  where
    conn_settings :: Connection.Settings
    conn_settings = Connection.settings (db_address  config)
                                        (fromIntegral . db_port $ config)
                                        (db_user     config)
                                        (db_password config)
                                        (db_name     config)

newtype AppM a =
  AppM
    { runAppM :: ReaderT (Pool) (Handler) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError ServerError)

instance MonadDB AppM where
  runSession sess =
    AppM $ do
      pool <- ask
      result <- liftIO $ use pool sess
      runAppM $ pure result

newtype AuthPool a =
  AuthPool
  {
    runAuthPool :: ReaderT Pool (ExceptT ServerError IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError ServerError)

instance MonadDB AuthPool where
  runSession sess = AuthPool $ do
    pool <- ask
    result <- liftIO $ use pool sess
    runAuthPool $ pure result

authPoolToExcept :: Pool -> AuthPool a -> ExceptT ServerError IO a
authPoolToExcept p v = runReaderT (runAuthPool v) p

