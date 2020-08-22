module DummySpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
        describe "Dummy Spec" $ do
                it "passed" $ do
                        True `shouldBe` True

