{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.UserEntity where

import Database.Persist.TH
import qualified Data.Text as T
import Data.Aeson

mkPersist sqlSettings [persistLowerCase|
User
    email T.Text
    firstname T.Text
    firstnameth T.Text
    gecos T.Text
    lastname T.Text
    lastnameth T.Text
    ouid T.Text
    roles [T.Text]
    uid T.Text
    username T.Text
    deriving Show
|]

instance ToJSON User where
        toJSON u =
                object [
                        "email" .= userEmail u,
                        "firstname" .= userFirstname u,
                        "firstnameth" .= userFirstnameth u,
                        "gecos" .= userGecos u,
                        "lastname" .= userLastname u,
                        "lastnameth" .= userLastnameth u,
                        "ouid" .= userOuid u,
                        "roles" .= userRoles u,
                        "uid" .= userUid u,
                        "username" .= userUsername u
                       ]
