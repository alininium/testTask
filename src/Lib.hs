module Lib
  ( server
  ) where

import Control.Exception (throw)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Aeson
import Data.Either
import Data.Int
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Profunctor
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic, Selector (selSourceStrictness))
import Hasql.Connection (Connection, ConnectionError)
import qualified Hasql.Connection as Connection
import Hasql.Encoders (noParams)
import Hasql.Decoders (Row, Value)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Pool
import Hasql.Session (QueryError, Session)
import qualified Hasql.Session as Session
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Typeable
import Data.Maybe
import Servant
import Servant.API.BasicAuth            (BasicAuthData (BasicAuthData))
import Types
import Spelling
import DB
import Control.Monad (when)
import qualified API
import Control.Monad.Except (runExceptT)
import qualified Data.Text.Internal.Read as T
import Network.Wreq (Auth)




api :: Proxy API
api = Proxy

context :: Proxy '[BasicAuthCheck User]
context = Proxy

instance Semigroup Int32 


authCheck :: Pool -> BasicAuthCheck User
authCheck p =
  let check (BasicAuthData username password) = do
        maybe_user_password <- authPoolToIO p $ getUserPassword (T.pack . BS.unpack $ username)
        let user_password = maybe "" snd maybe_user_password
        let user_id = maybe 0 fst maybe_user_password
        if (user_password /= "") && ((T.pack . BS.unpack) password == user_password)
            then return (Authorized (User user_id (T.pack . BS.unpack $ username)))
        else return Unauthorized
  in BasicAuthCheck check

basicAuthServerContext :: Pool -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext p = authCheck p :. EmptyContext

type API = API.GetAllUsersAPI
      :<|> API.GetAllPhrasesAPI
      :<|> API.GetPhrasesFromAPI
      :<|> API.GetPhrasesUnapprovedAPI
      :<|> API.UpdateApprovedAPI
      :<|> API.PostPhraseAPI


approveIfRightUser id force user = do
  root_user_id <- getRootAuthor id
  if fromJust root_user_id == user_id user
    then updateApproved id force
    else return "No access"


app :: Pool -> Application
app pool = serveWithContext api
                            (basicAuthServerContext pool) $
                            hoistServerWithContext api context appMToHandler $
                                                          const getUsers
                                                     :<|> const getPhrases
                                                     :<|> (\i _ -> getPhrasesFromUser i)
                                                     :<|> const getUnapprovedPhrases
                                                     :<|> approveIfRightUser
                                                     :<|> (\phrase force user -> checkSpelling phrase force >> postPhrase user phrase)

  where
    appMToHandler :: AppM a -> Handler a
    appMToHandler m = runReaderT (runAppM m) pool

server :: IO ()
server = do
  pool <- acquire 1 Nothing conn_settings
  run 8080 $ app pool
  where
    conn_settings :: Connection.Settings
    conn_settings = Connection.settings "localhost" 5432 "postgres" "postgres" "postgres"

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

authPoolToIO :: (Monoid a) => Pool -> AuthPool a -> IO a
authPoolToIO p m = fromRight mempty <$> runExceptT ( runReaderT (runAuthPool m) p)

