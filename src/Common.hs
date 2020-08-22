module Common (
        AppAction,
        ServerContext (..),
        AppScotty
        ) 
                where

import Web.Scotty.Trans
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import qualified Database.SQLite.Simple as SQL
import Control.Monad.Reader (ReaderT)

type Context = ReaderT ServerContext IO
type AppAction = ActionT L.Text Context
type AppScotty = ScottyT L.Text Context

data ServerContext = ServerContext {
      connection :: SQL.Connection,
      appId :: Text,
      appSecret :: Text
    }
