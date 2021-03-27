{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Model.MinUser where

import Data.Text as T
import Model.User as User
import GHC.Generics

data MinimalUser = MinimalUser {
                        ouid :: Text,
                        firstname :: Text,
                        lastname :: Text
                } deriving (Generic, Show, Eq)

fromMinimalUser :: MinimalUser -> User
fromMinimalUser (MinimalUser i fn ln) = 
        User {
                email = T.concat [i , "@student.chula.ac.th"],
                User.firstname = fn,
                firstnameth = fn,
                gecos = T.concat [ fn , " " , ln , ", ENG"],
                User.lastname = ln,
                lastnameth = ln,
                User.ouid = i,
                roles = ["student"],
                uid = T.concat [i , T.replicate (24 - T.length i) "f"],
                username = i
        }