
module DB
(
    getUsers,
    getPhrases,
    getPhrasesFromUser,
    getUnapprovedPhrases,
    getUserPassword,
    postPhrase,
    updateApproved, 
    getRootAuthor
) where

import Hasql.Pool
import Hasql.Session (QueryError, Session)
import Hasql.Decoders (Row, Value, text)
import Hasql.Encoders (noParams, bool)
import Data.Int
import Hasql.Statement
import Servant
import Data.ByteString.UTF8 (fromString)
import Data.Profunctor
import Control.Monad.Error.Class (MonadError)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Hasql.TH as TH
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class (MonadIO, liftIO)

import Types
import Data.Aeson (Value(Bool))
import Data.Maybe
import qualified API
import GHC.Base (VecElem(Int32ElemRep, Int64ElemRep))
import GHC.RTS.Flags (TraceFlags(user))

getUsers :: (MonadDB m, MonadError ServerError m) => m [User]
getUsers = _run () allUsers

getPhrases :: (MonadDB m, MonadError ServerError m) => m [Phrase]
getPhrases = _run () allPhrases

getUserPassword :: (MonadDB m, MonadError ServerError m) => T.Text -> m (Maybe (Int32, T.Text))
getUserPassword t = _run t userPassword

getPhrasesFromUser :: (MonadDB m, MonadError ServerError m) => Int32 -> m [Phrase]
getPhrasesFromUser i = _run i phrasesFromUser 

getUnapprovedPhrases :: (MonadDB m, MonadError ServerError m) => m [Phrase]
getUnapprovedPhrases = _run () unapprovedPhrases 


postPhrase :: (MonadDB m, MonadError ServerError m) => User -> API.PostPhrase -> m String
postPhrase user t = "Success" <$ do
        if isNothing $ API.reply_to t 
            then _run (API.text t, user_id user) insertPhrase 
            else _run (API.text t, user_id user, fromJust . API.reply_to $ t) insertPhraseReply

updateApproved :: (MonadDB m, MonadError ServerError m) => Int32 -> Maybe Bool -> m String
updateApproved i approved = "Success" <$ _run (fromMaybe True approved, i) markApproved
        
getRootAuthor :: (MonadDB m, MonadError ServerError m) => Int32 -> m (Maybe Int32)
getRootAuthor phrase_id = do
        res <- _run phrase_id rootAuthor
        if isJust res 
            then return res
            else _run phrase_id phraseAuthorById


_run :: (MonadDB m, MonadError ServerError m) => b -> Statement b a -> m a
_run v s = do
  result <- runSession getSession
  case result of
    Right result -> pure result
    Left error -> parseUsageError error
  where
    getSession = Session.statement v s
    parseUsageError r = throw500 (fromString $ show r)
    throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}
getListWith :: Statement a (V.Vector c) -> (c -> b) -> Statement a [b]
getListWith s fromTuple = rmap fromTuples s
    where 
      fromTuples vec = V.toList $ V.map fromTuple vec
      
allUsers :: Statement () [User]
allUsers = [TH.vectorStatement|
        select id:: int4, name::text
        from "author"|] `getListWith` tupleToUser

userPassword :: Statement T.Text (Maybe (Int32, T.Text))
userPassword = [TH.maybeStatement|
        select id::int4, password::text
        from "author"
        where name = $1::text|] 

allPhrases :: Statement () [Phrase]
allPhrases = [TH.vectorStatement|
        select id:: int4, text::text, author_id::int4, approved::bool
        from "phrase"|] `getListWith` tupleToPhrase

phrasesFromUser :: Statement Int32 [Phrase]
phrasesFromUser = [TH.vectorStatement|
        select id::int4, text::text, author_id::int4, approved::bool
        from "phrase"
        where author_id = $1 ::Int4|] `getListWith` tupleToPhrase

unapprovedPhrases :: Statement () [Phrase]
unapprovedPhrases = [TH.vectorStatement|
        select id::int4, text::text, author_id::int4, approved::bool
        from "phrase"
        where approved = false|] `getListWith` tupleToPhrase

  
insertPhrase :: Statement (T.Text, Int32) Int32
insertPhrase = [TH.singletonStatement|
    insert into "phrase" (text, author_id)
    values ($1::text, $2::int4)
    returning id::int4|]

insertPhraseReply :: Statement (T.Text, Int32, Int32) Int32
insertPhraseReply = [TH.singletonStatement|
    insert into "phrase" (text, author_id, parent_id)
    values ($1::text, $2::int4, $3::int4)
    returning id::int4|]

markApproved :: Statement (Bool, Int32) Int32
markApproved = [TH.singletonStatement|
    update "phrase" 
    set approved = $1::bool 
    where id = $2::int4
    returning 1::int4|]

rootAuthor :: Statement Int32 (Maybe Int32)
rootAuthor = [TH.maybeStatement|
    select A.author_id::int4 
    from phrase A, phrase B 
    where A.id = B.parent_id AND B.id = $1::int4 
|]

phraseAuthorById :: Statement Int32 (Maybe Int32)
phraseAuthorById = [TH.maybeStatement|
    select author_id::int4
    from phrase
    where id = $1::int4|]