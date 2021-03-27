module Common (
        AppAction,
        ServerContext (..),
        AppScotty,
        UserTicketStorageContext (..)
        ) 
                where

import Web.Scotty.Trans
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Control.Monad.Reader (ReaderT)
import Network.Wai.Handler.Warp (Port)
import Model.MinUser

type Context = ReaderT ServerContext IO
type AppAction = ActionT L.Text Context
type AppScotty = ScottyT L.Text Context

data ServerContext = ServerContext {
      appId :: String,
      appSecret :: String,
      port :: Port,
      ticketCtx :: UserTicketStorageContext
    }

data UserTicketStorageContext = UserTicketStorageContext {
        createTicketFromMUser :: MinimalUser -> IO Text,
        getMUserFromTicket :: Text -> IO (Maybe MinimalUser)
}