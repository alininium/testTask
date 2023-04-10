module Main (main) where

import           Lib
import           Configuration.Dotenv
import           System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

fromString :: String -> BS.ByteString
fromString =  T.encodeUtf8 . T.pack 

main :: IO ()
main = do
    _ <- loadFile defaultConfig
    db_address_env  <- fromString <$> getEnv "DB_ADDRESS"
    db_password_env <- fromString <$> getEnv "DB_PASSWORD"
    db_name_env     <- fromString <$> getEnv "DB_NAME"
    db_user_env     <- fromString <$> getEnv "DB_USER"
    db_port_env     <- read <$> getEnv "DB_PORT"
    server_port_env <- read <$> getEnv "SERVER_PORT"
    pool_size_env   <- read <$> getEnv "POOL_SIZE"
    server ServerConfig {
        pool_size   = pool_size_env,
        server_port = server_port_env,
        db_address  = db_address_env,
        db_password = db_password_env,
        db_name     = db_name_env,
        db_user     = db_user_env,
        db_port     = db_port_env
    }