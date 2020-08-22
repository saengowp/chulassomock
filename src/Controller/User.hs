module Controller.User (route) where

import Common
import Web.Scotty.Trans
import qualified Repository.User as Repo
import Model.User
import Control.Monad.Trans.Reader (asks, ask)
import Control.Monad.IO.Class
import System.Random (randomRIO)
import Data.Text as T
import Data.Text.Lazy as LT
import Data.Char (chr, ord)
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (status401)

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
            parsedService = lookup "service" ps
        case (parsedUser, parsedService) of
           ((Just usr), (Just srv)) -> do
                ticket <- createUser usr
                redirect $ LT.concat [ srv, "?ticket=", LT.fromStrict ticket ]
           (_, (Just srv)) -> do
                redirect $ LT.concat [ "/html/login.html?service=", srv ]
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
        con <- lift $ asks connection
        ServerContext { appId = aid, appSecret = as } <- lift ask
        case (sequence . Prelude.map (\x -> fmap LT.toStrict . lookup x $ hs) $ ["DeeAppId", "DeeAppSecret", "DeeTicket"]) of
          Just [ raid, ras, ticket ]
            | raid == aid && ras == as -> do
                    user <- liftIO $ Repo.get con ticket
                    case user of
                      Just u -> json u
                      Nothing -> reject
          _ -> reject
        return ()

hexChar :: Int -> Char
hexChar v
        | v < 10 = chr (ord '0' + v)
        | otherwise = chr (ord 'a' + v - 10)

createUser :: MinimalUser -> AppAction T.Text
createUser user = do
        ticketId <- liftIO . fmap T.pack . replicateM 24 . fmap hexChar . randomRIO $ (0 :: Int,16 :: Int)
        con <- lift $ asks connection
        liftIO $ Repo.add con (fromMinimalUser user) ticketId
        return ticketId

        
