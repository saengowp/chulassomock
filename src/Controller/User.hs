module Controller.User (route) where

import Common
import Web.Scotty.Trans
import Control.Monad.Trans.Reader (asks, ask)
import Control.Monad.IO.Class
import System.Random (randomRIO)
import Data.Text as T
import Data.Text.Lazy as LT
import Data.Char (chr, ord)
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (status401, status400)
import Network.HTTP.Types.URI (QueryItem, parseQuery, renderQuery, urlEncode)
import Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Network.URI as URI
import Model.MinUser
import TicketStorage.Base
import Data.ByteString.Char8 as BS8
import Control.Monad.Trans.Maybe
import Data.Maybe

route :: AppScotty ()
route = do
        get "/login" login
        post "/login" login
        get "/serviceValidation"  serviceValidation
        post "/serviceValidation" serviceValidation

addQueryParam :: QueryItem -> URI.URI -> URI.URI
addQueryParam q u = u {URI.uriQuery = BS8.unpack . renderQuery True $ newQueries}
        where orgQueries = parseQuery . BS8.pack . URI.uriQuery $  u
              newQueries = orgQueries ++ [q]

login :: AppAction ()
login = do
        ticketContext <- lift $ asks ticketCtx
        ps <- params
        let [mouid, mfirstname, mlastname] =
                Prelude.map (\key -> LT.toStrict <$> lookup key ps ) ["ouid", "firstname", "lastname"]
            mUser = MinimalUser <$> mouid <*> mfirstname <*> mlastname
            mService = lookup "service" ps >>= URI.parseURI . LT.unpack 
            uriToText u = LT.pack $ URI.uriToString id u "" :: LT.Text
        case (mUser, mService) of
           (Just usr, Just srv) -> do
                ticket <- liftIO $ createTicketFromMUser ticketContext usr
                let ticketQuery = ("ticket", Just $ BS8.pack . T.unpack $ ticket)
                    redirectUrl = uriToText . addQueryParam ticketQuery $ srv
                redirect redirectUrl
           (_, Just srv) -> do
                let encodedServiceUrl = LT.pack . URI.escapeURIString URI.isUnescapedInURIComponent . LT.unpack . uriToText $ srv
                redirect $ LT.concat [ "/html/login.html?service=", encodedServiceUrl]
           _ -> do
                   status status400 
                   text "No service specified"

reject :: AppAction ()
reject = do
           setHeader "Content-Type" "application/json; charset=utf-8"
           status status401
           raw "{ \
                  \ \"type\" : \"error\", \
                  \ \"content\" : \"invalid ticket/permission\" \
                \ }"
           finish

serviceValidation :: AppAction ()
serviceValidation = do
        hs <- headers
        ServerContext { appId = appid, appSecret = appsecret, ticketCtx = ticketC} <- lift ask
        user <- runMaybeT $ do
                let assertV v = if v then return () else MaybeT (return Nothing)
                    lookupH h = MaybeT . return $ lookup h hs
                hAppId <- lookupH "DeeAppId"
                assertV $ hAppId == LT.pack appid
                hAppSecret <- lookupH "DeeAppSecret"
                assertV $ hAppSecret == LT.pack appsecret
                hTicket <- lookupH "DeeTicket"
                MaybeT . liftIO $ getMUserFromTicket ticketC (T.pack . LT.unpack $ hTicket)
        maybe reject (json . fromMinimalUser) user

        
