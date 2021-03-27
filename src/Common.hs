module Common (
        AppAction,
        ServerContext (..),
        AppScotty
        ) 
                where

import Web.Scotty.Trans
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Control.Monad.Reader (ReaderT)
import Network.Wai.Handler.Warp (Port)

type Context = ReaderT ServerContext IO
type AppAction = ActionT L.Text Context
type AppScotty = ScottyT L.Text Context

data ServerContext = ServerContext {
      appId :: String,
      appSecret :: String,
      port :: Port
    }
