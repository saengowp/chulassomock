module Mock (appCtx) where

import Common
import TicketStorage.OnTicket

appCtx :: IO ServerContext
appCtx = do
        ns <- onTicketStore 
        return $ ServerContext {
                        appSecret = "APPSECRET",
                        appId = "APPID",
                        port = 8080,
                        ticketCtx = ns
                }
