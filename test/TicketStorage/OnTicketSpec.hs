module TicketStorage.OnTicketSpec (spec) where

import Test.Hspec (it, Spec, shouldBe)
import TicketStorage.OnTicket (onTicketStore)
import Common
import Model.MinUser

spec :: Spec
spec = do
    it "Encode and Decode user" $ do
        let u = MinimalUser "0000000000" "Alice" "Bob"
        (UserTicketStorageContext u2t t2u) <- onTicketStore
        t <- u2t u
        putStrLn "Gen ticket"
        putStrLn . show $ t
        u2 <- t2u t
        u2 `shouldBe` (Just u)
    it "Decode Invalid User" $ do
        (UserTicketStorageContext _ t2u) <- onTicketStore
        r <- t2u "BAAAAAAAAAowMDAwMDAwMDAwAAAAAAAAAAVBbGljZQAAAAAAAAADQm9iAAAAAAAAABSdr0Z19KZwUlDmNcRQ6STMQEdkCA=="
        r `shouldBe` Nothing