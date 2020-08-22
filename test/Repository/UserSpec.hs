{-# LANGUAGE OverloadedStrings #-}

module Repository.UserSpec (spec) where

import Test.Hspec
import Repository.User
import Model.User
import Database.SQLite.Simple as SQL

spec :: Spec
spec = describe "User Repository" $ do
        it "Add and Retrieve user" $ do
                let usr = fromMinimalUser $ MinimalUser { m_ouid = "1234567890", m_firstname = "Elohim", m_lastname = "Weir" }
                con <- SQL.open ":memory:"
                initDb con
                add con usr "TestTicket"
                gusr <- get con "TestTicket"
                gusr `shouldBe` (Just usr)
        it "Incorrect Ticket" $ do
                con <- SQL.open ":memory:"
                initDb con
                usr <- (get con "NonExistTicket")
                usr `shouldBe` Nothing

                
