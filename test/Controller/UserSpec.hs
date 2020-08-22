module Controller.UserSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Lib (app)
import Data.Text as T
import Data.Text.Encoding as E
import Network.Wai.Test (SResponse (SResponse))
import Network.HTTP.Types.Method
import Data.Aeson
import Data.Aeson.Types

import Controller.Mock

spec :: Spec
spec = with (appCtx >>= app) $ do
        describe "E2E App Testing" $ do
                it "Ident Flow" $ do
                        get "/login?service=http://example.com" `shouldRespondWith` 302 {matchHeaders = ["Location" <:> "/html/login.html?service=http://example.com"]}
                it "Submit Login and Validate Flow" $ do
                        respLogin <- postHtmlForm "/login" [
                                ("service", "http://example.com"),
                                ("ouid", "1234567890"),
                                ("firstname","John"),
                                ("lastname","Doe")]
                        let (SResponse _ headers _) = respLogin
                        (pure respLogin) `shouldRespondWith` 302
                        ticket <- case (lookup "Location" headers) >>= ((stripPrefix "http://example.com?ticket=") . E.decodeUtf8) of
                                        Just s -> return s
                                        Nothing -> (liftIO $ expectationFailure ("No ticket Headers:" ++ show headers)) >> pure "NOTHING"

                        (SResponse _ _ j) <- request methodGet "serviceValidation" [
                                                ("DeeAppId", "APPID"),
                                                ("DeeAppSecret", "APPSECRET"),
                                                ("DeeTicket", E.encodeUtf8 ticket)] ""
                        case decode j of
                          Just (Object obj) -> liftIO $ do
                                  case parse (\o -> sequence [(o .: "ouid"), (o.: "firstname"), (o.: "lastname")]) obj of
                                    (Success [ouid, fn, ln]) -> liftIO $ do
                                          ouid `shouldBe` ("1234567890" :: Text)
                                          fn `shouldBe` ("John" :: Text)
                                          ln `shouldBe` ("Doe" :: Text)
                                    _ -> liftIO $ expectationFailure $ "JSON No propeties"
                          _ -> liftIO . expectationFailure $ "Incorrect data from validation (not json object)"
                        
                it "Login with no service" $ do
                        get "/login" `shouldRespondWith` 200

                it "Invalid Ticket" $ do
                        request methodGet "/serviceValidation" [
                                                ("DeeAppId", "APPID"),
                                                ("DeeAppSecret", "APPSECRET"),
                                                ("DeeTicket", "123456")] "" `shouldRespondWith` 401
                it "Invalid AppInfo" $ do
                        request methodGet "/serviceValidation" [
                                                ("DeeAppId", "APPIDJJJJJ"),
                                                ("DeeAppSecret", "APPSECRET"),
                                                ("DeeTicket", "123456")] "" `shouldRespondWith` 401




