module Controller.Mock (appCtx) where

import Common

appCtx :: IO ServerContext
appCtx = do
        return $ ServerContext {
                        appSecret = "APPSECRET",
                        appId = "APPID",
                        port = 8080
                }
