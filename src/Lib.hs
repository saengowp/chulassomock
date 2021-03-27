{-# LANGUAGE OverloadedStrings #-}

module Lib (main, app) where

import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import Web.Scotty.Trans
import Control.Monad.Reader (runReaderT)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application)
import Data.Maybe
import Data.Text.Lazy
import qualified Data.Text as T

import Common
import qualified Controller.User as UserC (route)
import qualified Controller.LoginHtml as LoginC (route)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import TicketStorage.OnTicket

hello :: AppAction ()
hello = text "ChulaSSO Mock Server"
    
app :: ServerContext -> IO Application
app srvCtx = do
        scottyAppT (`runReaderT` srvCtx) $ do
                 middleware logStdoutDev
                 get "/" hello
                 UserC.route
                 LoginC.route

getEnvDef :: String -> String -> IO String
getEnvDef env def = fromMaybe def <$> lookupEnv env

serverContextFromEnv :: IO ServerContext
serverContextFromEnv = ServerContext <$> 
                                getEnvDef "APPID" "APPID" <*> 
                                getEnvDef "APPSECRET" "APPSECRET" <*>
                                (read <$> getEnvDef "PORT" "8080") <*>
                                onTicketStore 

main :: IO ()
main = do
        srvCtx <- serverContextFromEnv
        waiApp <- app srvCtx
        putStrLn $ "Starting server on port " ++ show (port srvCtx)
        hFlush stdout
        run (port srvCtx) waiApp
