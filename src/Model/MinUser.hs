{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Model.MinUser where

import Data.Text as T
import Data.Aeson
import GHC.Generics
import Faker as FK
import qualified Faker.Name as F
import Control.Monad (replicateM)
import System.Random (randomRIO, newStdGen)
import Model.UserEntity

data MinimalUser = MinimalUser {
        ouid :: Text,
        firstname :: Text,
        lastname :: Text
                               }
                               deriving Show

fromMinimalUser :: MinimalUser -> User
fromMinimalUser MinimalUser{ouid = i, firstname = fn, lastname = ln} = 
        User {
                userEmail = T.concat [i , "@student.chula.ac.th"],
                userFirstname = fn,
                userFirstnameth = fn,
                userGecos = T.concat [ fn , " " , ln , ", ENG"],
                userLastname = ln,
                userLastnameth = ln,
                userOuid = i,
                userRoles = ["student"],
                userUid = T.concat [ i , (T.replicate (24 - T.length i) "f")],
                userUsername = i
        }

genMinUser :: IO MinimalUser
genMinUser = do
        nms <- splitOn " " <$> FK.generateWithSettings (FK.setNonDeterministic FK.defaultFakerSettings) F.name 
        let fn = Prelude.head nms
            ln = T.intercalate " " $ Prelude.tail nms
        let genId :: IO Text
            genId = pack <$> (replicateM 10 (randomRIO ('0', '9')))
        i <- genId
        return $ MinimalUser {firstname = fn, lastname = ln, ouid = i}
