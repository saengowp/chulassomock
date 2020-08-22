{-# LANGUAGE OverloadedStrings #-}

module Model.UserSpec (spec) where

import Test.Hspec
import Model.User
import qualified Data.ByteString.Lazy as B
import Data.Aeson

exMinUser :: MinimalUser
exMinUser = MinimalUser {
        m_ouid = "6200000000",
        m_firstname = "Steve",
        m_lastname = "McBoat"
                     }
exUserJson :: IO B.ByteString
exUserJson = B.readFile "test/Model/user.json"

spec :: Spec
spec = describe "User" $ do
        it "User from MinimalUser correct filling" $ do
                exUser <- decode <$> exUserJson :: IO (Maybe User)
                case exUser of
                  Just usr -> (fromMinimalUser exMinUser) `shouldBe` usr
                  _ -> expectationFailure "UserJSON decode fail"
        it "Gen Minimal User No Fail" $ do
                _ <- genMinUser
                True `shouldBe` True
                
