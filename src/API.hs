module API
(
    PostPhrase (..),
    GetAllUsersAPI,
    GetAllPhrasesAPI,
    GetPhrasesFromAPI,
    GetPhrasesUnapprovedAPI,
    UpdateApprovedAPI,
    PostPhraseAPI,
    toResponse,
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

data Response a = Response { status :: T.Text,
                             result :: a
} deriving (Generic)

instance ToJSON PostPhrase
instance FromJSON PostPhrase

instance ToJSON a => ToJSON (Response a)
instance FromJSON a => FromJSON (Response a)

toResponse :: a -> Response a
toResponse a = Response {status = "OK", result = a}

type GetAllUsersAPI          = "api" :> "v1" :> "users"   :>                            BasicAuth "basic-realm" User :> Get '[JSON] (Response [User])
type GetAllPhrasesAPI        = "api" :> "v1" :> "phrases" :>                            BasicAuth "basic-realm" User :> Get '[JSON] (Response [Phrase])
type GetPhrasesFromAPI       = "api" :> "v1" :> "phrases" :> Capture "user_id" Int32 :> BasicAuth "basic-realm" User :> Get '[JSON] (Response [Phrase])
type GetPhrasesUnapprovedAPI = "api" :> "v1" :> "phrases" :> "unapproved" :>            BasicAuth "basic-realm" User :> Get '[JSON] (Response [Phrase])

type UpdateApprovedAPI       = "api" :> "v1" :> "phrases" :> "approve" :> Capture "phrase_id" Int32      :> QueryParam "approved" Bool :> BasicAuth "basic-realm" User :> Put  '[JSON] (Response ())
type PostPhraseAPI           = "api" :> "v1" :> "phrases"              :> ReqBody '[JSON] API.PostPhrase :> QueryParam "force"    Bool :> BasicAuth "basic-realm" User :> Post '[JSON] (Response ())

