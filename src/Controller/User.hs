module Controller.User (route) where

import Common
import Web.Scotty.Trans
import Control.Monad.Trans.Reader (asks, ask)
import Control.Monad.IO.Class
import Data.Text as T
import Data.Text.Lazy as LT
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (status401, status400)
import Network.HTTP.Types.URI (QueryItem, parseQuery, renderQuery)
import qualified Network.URI as URI
import Model.MinUser
import Data.ByteString.Char8 as BS8
import Control.Monad.Trans.Except
import Data.Aeson

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
                   text "No service specified or service is not an URL"

data Rejection = Rejection String String String

instance ToJSON Rejection where
        toJSON (Rejection rt rc rh) =
                object ["type" .= rt, "content" .= rc, "hint" .= rh]

reject :: String -> AppAction ()
reject reason = do
           setHeader "Content-Type" "application/json; charset=utf-8"
           status status401
           let res = Rejection "error" "invalid ticket/permission" reason
           raw $ encode res 
           finish

serviceValidation :: AppAction ()
serviceValidation = do
        hs <- headers
        ServerContext { appId = appid, appSecret = appsecret, ticketCtx = ticketC} <- lift ask
        user <- runExceptT $ do
                let assertV expected actual valname = 
                        if expected == actual 
                                then return () 
                                else throwE ("Assertion failed: expect " ++ expected ++ " got " ++ valname)
                    lookupH h = maybe (throwE ("Header " ++ LT.unpack h ++ " not found")) return $ lookup h hs
                hAppId <- lookupH "DeeAppId"
                assertV appid (LT.unpack hAppId) "AppId"
                hAppSecret <- lookupH "DeeAppSecret"
                assertV appsecret (LT.unpack hAppSecret) "AppSecret"
                hTicket <- lookupH "DeeTicket"
                u <- liftIO $ getMUserFromTicket ticketC (T.pack . LT.unpack $ hTicket)
                maybe (throwE "Can't retrieve the user from the given ticket") return u
        either reject (Web.Scotty.Trans.json . fromMinimalUser) user

        
