module Types 
 (
    User (..),
    Phrase (..),
    MonadDB (..), 
    tupleToPhrase,
    tupleToUser
 ) where

import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Text as T
import Hasql.Session (Session)
import Hasql.Pool (UsageError)
import Data.Int

data User =
  User
    { user_id :: Int32
    , name :: T.Text
    }
  deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data Phrase = 
  Phrase 
    { phrase_id :: Int32,
      phrase_text :: T.Text,
      phrase_author_id :: Int32,
      phrase_approved :: Bool
    }
  deriving (Show, Generic)

tupleToUser :: (Int32, T.Text) -> User
tupleToUser (id, name) = User id name 

tupleToPhrase :: (Int32, T.Text, Int32, Bool) -> Phrase
tupleToPhrase (id, text, author, approved) = Phrase id text author approved

instance FromJSON Phrase
instance ToJSON Phrase

class MonadIO m =>
      MonadDB m
  where
  runSession :: Session a -> m (Either UsageError a)


