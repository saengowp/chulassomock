module Controller.Mock (appCtx) where

import Common
import Database.SQLite.Simple (open)

appCtx :: IO ServerContext
appCtx = do
        con <- open ":memory:"
        return $ ServerContext {
                connection = con,
                appSecret = "APPSECRET",
                appId = "APPID"
                               }
