module Spelling (
    checkSpelling
) where


import qualified API
import Network.Wreq
import qualified Data.Text as T
import Control.Monad.Error.Class (MonadError)
import Servant
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Codec.Binary.UTF8.String (utf8Encode)


url :: String -> String
url = ("https://speller.yandex.net/services/spellservice.json/checkText?text="++)

checkSpelling :: (MonadError ServerError m, MonadIO m) => API.PostPhrase -> Maybe Bool -> m Bool
checkSpelling a force = if fromMaybe False force then return True else (do
    response <- liftIO . get . url . utf8Encode . T.unpack . API.text $ a
    let responseCode = head $ response ^.. responseStatus . statusCode
    let body = response ^. responseBody
    when (responseCode /= 200) (throw err500 "Yandex server is currently down")
    if body == "[]"
        then return True
        else throw err400 "Spelling is incorrect"
    )
    where
        throw e msg = throwError e {errBody = LBS.fromStrict msg}