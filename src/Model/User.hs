{-# LANGUAGE DeriveGeneric #-}

module Model.User where
import Data.Aeson
import Data.Text
import GHC.Generics

data User = User {
        email :: Text,
        firstname :: Text,
        firstnameth :: Text,
        gecos :: Text,
        lastname :: Text,
        lastnameth :: Text,
        ouid :: Text,
        roles :: [Text],
        uid :: Text,
        username :: Text
} deriving (Generic, Show)

instance ToJSON User

instance FromJSON User

