module HelloSpec (spec) where

import Test.Hspec (it, Spec)
import Test.Hspec.Wai (with, shouldRespondWith, get)
import Mock (appCtx)
import Lib (app)

spec :: Spec
spec = with (appCtx >>= app) $ do
        it "Hello" $ do
            get "/" `shouldRespondWith` 200
