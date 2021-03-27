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
import Network.HTTP.Types.Status (status401)
import qualified Network.HTTP.Types.URI as TURI
import Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Network.URI as URI
import Model.MinUser

route :: AppScotty ()
route = do
        get "/login" login
        post "/login" login
        get "/serviceValidation"  serviceValidation
        post "/serviceValidation" serviceValidation

login :: AppAction ()
login = do
        ps <- params
        let parsedUser = MinimalUser <$> 
                (fmap LT.toStrict . lookup "ouid" $ ps) <*> 
                (fmap LT.toStrict . lookup "firstname" $ ps) <*> 
                (fmap LT.toStrict . lookup "lastname" $ ps)
            parsedService = lookup "service" ps >>= (URI.parseURI . LT.unpack) :: Maybe URI.URI
            uriToText u = LT.pack $ URI.uriToString id u "" :: LT.Text
        case (parsedUser, parsedService) of
           (Just usr, Just srv) -> do
                ticket <- createUser usr
                let redirectUrl = uriToText newUri :: LT.Text
                        where
                           ticketQuery = ("ticket", Just . TE.encodeUtf8 $ ticket)
                           orgQueries = TURI.parseQuery . TE.encodeUtf8 . T.pack . URI.uriQuery $ srv
                           newQueries = orgQueries ++ [ ticketQuery ]
                           newUri = srv { URI.uriQuery = T.unpack . TE.decodeUtf8 . TURI.renderQuery True $ newQueries }
                redirect redirectUrl
           (_, Just srv) -> do
                redirect $ LT.concat [ "/html/login.html?service=", LT.pack . URI.escapeURIString URI.isUnescapedInURIComponent . LT.unpack . uriToText $ srv ]
           _ -> text "No service specified"

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
        -- repo <- lift $ asks userRepo
        ServerContext { appId = aid, appSecret = as } <- lift ask
        {- case (sequence . Prelude.map (\x -> fmap LT.toStrict . lookup x $ hs) $ ["DeeAppId", "DeeAppSecret", "DeeTicket"]) of
          Just [ raid, ras, ticket ]
            | raid == aid && ras == as -> do
                    user <- liftIO . fromTicket repo $ ticket
                    case user of
                      Just u -> json u
                      Nothing -> reject
          _ -> reject -}
        return ()

hexChar :: Int -> Char
hexChar v
        | v < 10 = chr (ord '0' + v)
        | otherwise = chr (ord 'a' + v - 10)

createUser :: MinimalUser -> AppAction T.Text
createUser user = do
        -- ticketId <- liftIO . fmap T.pack . replicateM 24 . fmap hexChar . randomRIO $ (0 :: Int,16 :: Int)
        {- repo <- lift $ asks userRepo
        ticketId <- liftIO . createTicket repo $ (fromMinimalUser user)
        return ticketId -}
        return "eeee"

        
