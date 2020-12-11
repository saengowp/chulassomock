{-# LANGUAGE MultiParamTypeClasses #-}

module Repository.UserRepo where

import Model.UserEntity
import Data.Text
import Control.Monad.IO.Class

data UserRepo = UserRepo {
        createTicket :: User -> IO Text,
        fromTicket :: Text -> IO (Maybe User),
        recentUsers :: IO [User]
        }
