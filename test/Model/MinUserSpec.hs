module Model.MinUserSpec (spec) where

import Test.Hspec (Spec, it, shouldBe)
import Model.MinUser (MinimalUser(MinimalUser), fromMinimalUser)
import Model.User

exMinUser :: MinimalUser 
exMinUser = MinimalUser "6200000021" "John" "Doe"

exUser :: User
exUser = User {
    email = "6200000021@student.chula.ac.th",
    firstname = "John",
    firstnameth = "John",
    gecos = "John Doe, ENG",
    lastname = "Doe",
    lastnameth = "Doe",
    ouid = "6200000021",
    roles = ["student"],
    uid = "6200000021ffffffffffffff",
    username = "6200000021"
}


spec :: Spec
spec = do 
        it "Minimal User convert to User" $ do
            fromMinimalUser exMinUser `shouldBe` exUser