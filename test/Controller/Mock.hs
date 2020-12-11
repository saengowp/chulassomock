module Controller.Mock (appCtx) where

import Common
import Repository.MockUserRepo

appCtx :: IO ServerContext
appCtx = do
        return $ ServerContext {
                userRepo = mockUserRepo,
                appSecret = "APPSECRET",
                appId = "APPID"
                               }
