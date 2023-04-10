{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

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

import Data.Int
import Hasql.Statement
import Hasql.Session
import Servant
import Data.ByteString.UTF8 (fromString)
import Data.Profunctor
import Control.Monad.Error.Class (MonadError)
import Control.Monad (void)
import qualified Hasql.Session as Session
import qualified Hasql.TH as TH
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as LBS

import Types
import Data.Maybe
import qualified API

getUsers :: (MonadDB m, MonadError ServerError m) => m [User]
getUsers = _run_simple () allUsers

getPhrases :: (MonadDB m, MonadError ServerError m) => m [Phrase]
getPhrases = _run_simple () allPhrases

getUserPassword :: (MonadDB m, MonadError ServerError m) => T.Text -> m (Int32, T.Text)
getUserPassword t = do
    creds <- _run_simple t userPassword
    case creds of
        Just result -> return result
        Nothing     -> throwError err500 {errBody = LBS.fromStrict "Couldn't find this user"}

getPhrasesFromUser :: (MonadDB m, MonadError ServerError m) => Int32 -> m [Phrase]
getPhrasesFromUser i = _run_simple i phrasesFromUser

getUnapprovedPhrases :: (MonadDB m, MonadError ServerError m) => m [Phrase]
getUnapprovedPhrases = _run_simple () unapprovedPhrases


postPhrase :: (MonadDB m, MonadError ServerError m) => User -> API.PostPhrase -> m ()
postPhrase user t = Control.Monad.void (_run_simple (API.text t, user_id user, API.reply_to t) insertPhrase)

updateApproved :: (MonadDB m, MonadError ServerError m) => Int32 -> Maybe Bool -> m ()
updateApproved i approved = Control.Monad.void (_run_simple (fromMaybe True approved, i) markApproved)

getRootAuthor :: (MonadDB m, MonadError ServerError m) => Int32 -> m Int32
getRootAuthor p_id = do
    result <- _run p_id session
    case result of
        Just u_id -> return u_id
        Nothing   -> throwError err500 {errBody = LBS.fromStrict "Couldn't find the author"}
    where session :: Int32 -> Session (Maybe Int32)
          session x = do
                root_author <- Session.statement x rootAuthor
                case root_author of
                    Just result -> return (Just result)
                    Nothing     -> Session.statement x phraseAuthorById

simpleSession :: Statement params result -> params -> Session result
simpleSession a = (`Session.statement` a)

_run_simple :: (MonadDB m, MonadError ServerError m) => params -> Statement params result -> m result
_run_simple arg sess = _run arg (simpleSession sess)

_run :: (MonadDB m, MonadError ServerError m) => params -> (params -> Session result) -> m result
_run arg sess = do
  result <- runSession (sess arg)
  case result of
    Right usage_result -> pure usage_result
    Left usage_error -> parseUsageError usage_error
  where
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

insertPhrase:: Statement (T.Text, Int32, Maybe Int32) Int32
insertPhrase= [TH.singletonStatement|
    insert into "phrase" (text, author_id, parent_id)
    values ($1::text, $2::int4, $3::int4?)
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