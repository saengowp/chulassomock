module Mock (appCtx) where

import Common
import TicketStorage.Base

appCtx :: IO ServerContext
appCtx = do
        ns <- nothingStorage
        return $ ServerContext {
                        appSecret = "APPSECRET",
                        appId = "APPID",
                        port = 8080,
                        ticketCtx = ns
                }
