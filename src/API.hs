module API
(
    PostPhrase (..),
    GetAllUsersAPI,
    GetAllPhrasesAPI,
    GetPhrasesFromAPI,
    GetPhrasesUnapprovedAPI,
    UpdateApprovedAPI,
    PostPhraseAPI,
) where

import Data.Int
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text as T
import Servant
import Types

data PostPhrase = PostPhrase { text :: T.Text
                             , reply_to :: Maybe Int32
} deriving Generic

instance ToJSON PostPhrase
instance FromJSON PostPhrase

type GetAllUsersAPI          = "api" :> "v1" :> "users"   :>                            BasicAuth "basic-realm" User :> Get '[JSON] [User]
type GetAllPhrasesAPI        = "api" :> "v1" :> "phrases" :>                            BasicAuth "basic-realm" User :> Get '[JSON] [Phrase]
type GetPhrasesFromAPI       = "api" :> "v1" :> "phrases" :> Capture "user_id" Int32 :> BasicAuth "basic-realm" User :> Get '[JSON] [Phrase]
type GetPhrasesUnapprovedAPI = "api" :> "v1" :> "phrases" :> "unapproved" :>            BasicAuth "basic-realm" User :> Get '[JSON] [Phrase]

type UpdateApprovedAPI       = "api" :> "v1" :> "phrases" :> "approve" :> Capture "phrase_id" Int32      :> QueryParam "approved" Bool :> BasicAuth "basic-realm" User :> Put  '[JSON] String
type PostPhraseAPI           = "api" :> "v1" :> "phrases"              :> ReqBody '[JSON] API.PostPhrase :> QueryParam "force"    Bool :> BasicAuth "basic-realm" User :> Post '[JSON] String

