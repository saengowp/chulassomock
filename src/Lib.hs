{-# LANGUAGE OverloadedStrings #-}

module Lib (main, app) where

import System.Environment (getEnv)
import System.IO.Error (catchIOError)
import System.IO (hFlush, stdout)
import Web.Scotty.Trans
import Control.Monad.Reader (runReaderT)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application)
import Data.Text.Lazy
import qualified Data.Text as T

import Common
import qualified Controller.User as UserC (route)
import qualified Controller.LoginHtml as LoginC (route)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Repository.MockUserRepo

getEnvDef :: String -> String -> IO String
getEnvDef env def = catchIOError (getEnv env) (\_ -> pure def)


hello :: AppAction ()
hello = text "ChulaSSO Mock Server"
    
app :: ServerContext -> IO Application
app srvCtx = do
        scottyAppT (\response -> runReaderT response srvCtx) $ do
                 middleware logStdoutDev
                 get "/" hello
                 UserC.route
                 LoginC.route


main :: IO ()
main = do
        port <- read <$> getEnvDef "PORT" "8080"
        e_appId <- T.pack <$> getEnvDef "APPID" "APPID"
        e_appSecret <- T.pack <$> getEnvDef "APPSECRET" "APPSECRET"
        let srvCtx = ServerContext { userRepo = mockUserRepo, appId = e_appId, appSecret = e_appSecret}
        a <- app srvCtx
        putStrLn $ "Starting server on port " ++ show port
        hFlush stdout
        run port a
