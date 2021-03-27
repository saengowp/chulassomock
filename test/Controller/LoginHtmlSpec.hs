module Controller.LoginHtmlSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Lib (app)
import Mock (appCtx)

spec :: Spec
spec = with (appCtx >>= app) $ do
        describe "Login Html" $ do
                it "Reachable" $ do
                        get "/html/login.html?service=w" `shouldRespondWith` 200
