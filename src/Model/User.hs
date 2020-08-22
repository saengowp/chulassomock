{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Model.User (
        User (..), 
        MinimalUser (..), 
        fromMinimalUser,
        genMinUser) where

import Data.Text as T
import Data.Aeson
import GHC.Generics
import Faker as FK
import qualified Faker.Name as F
-- import Data.Time.Clock.POSIX (getPOSIXTime)
-- import Codec.Base16 as B16
import Control.Monad (replicateM)
-- import System.Random.Stateful (uniformRM, newIOGenM)
import System.Random (randomRIO, newStdGen)

data User = User {
        email :: Text , -- ouid@student.chula.ac.th
        firstname :: Text,
        firstnameth :: Text,
        gecos :: Text, -- "firstname lastname, ENG"
        lastname :: Text,
        lastnameth :: Text,
        ouid :: Text,
        roles :: [Text], -- [ "student"]
        uid :: Text, -- 24 hex objectId
        username :: Text -- ouid
                 }
                 deriving (Generic, Show, Eq)

instance FromJSON User

instance ToJSON User where
        toEncoding = genericToEncoding defaultOptions

data MinimalUser = MinimalUser {
        m_ouid :: Text,
        m_firstname :: Text,
        m_lastname :: Text
                               }
                               deriving Show

fromMinimalUser :: MinimalUser -> User
fromMinimalUser MinimalUser{m_ouid = i, m_firstname = fn, m_lastname = ln} = 
        User {
                email = T.concat [i , "@student.chula.ac.th"],
                firstname = fn,
                firstnameth = fn,
                gecos = T.concat [ fn , " " , ln , ", ENG"],
                lastname = ln,
                lastnameth = ln,
                ouid = i,
                roles = ["student"],
                uid = T.concat [ i , (T.replicate (24 - T.length i) "f")],
                username = i
        }

--(B16.encode :: ByteString -> Text) <$> ((genByteString 12) <$> getStdGen)

genMinUser :: IO MinimalUser
genMinUser = do
        nms <- splitOn " " <$> FK.generateWithSettings (FK.setNonDeterministic FK.defaultFakerSettings) F.name 
        let fn = Prelude.head nms
            ln = T.intercalate " " $ Prelude.tail nms
        let genId :: IO Text
            genId = pack <$> (replicateM 10 (randomRIO ('0', '9')))
        i <- genId
        return $ MinimalUser {m_firstname = fn, m_lastname = ln, m_ouid = i}

