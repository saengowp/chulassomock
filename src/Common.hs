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
import Repository.UserRepo (UserRepo)

type Context = ReaderT ServerContext IO
type AppAction = ActionT L.Text Context
type AppScotty = ScottyT L.Text Context

data ServerContext = ServerContext {
      userRepo :: UserRepo,
      appId :: Text,
      appSecret :: Text
    }
