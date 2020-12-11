module Repository.MockUserRepo (mockUserRepo) where

import Repository.UserRepo
import Model.UserEntity

fakeUser :: User
fakeUser = User {
                userEmail = "email",
                userFirstname = "firstname",
                userFirstnameth = "firstnameth",
                userGecos = "gecos",
                userLastname = "lastname",
                userLastnameth = "lastnameth",
                userOuid = "ouid",
                userRoles = ["roles"],
                userUid = "uid",
                userUsername = "username"
                }

mockUserRepo :: UserRepo
mockUserRepo = UserRepo {
                createTicket =  \_ -> pure "ticket",
                fromTicket = \_ -> pure $ Just fakeUser,
                recentUsers = pure [fakeUser]
                }
