{-# Language OverloadedStrings #-}

module Controller.UserSpec (spec) where

import Test.Hspec
    ( describe, it, expectationFailure, shouldBe, Spec )
import Test.Hspec.Wai
import Lib (app)
import Data.Text as T
import Data.ByteString as BS
import Data.Text.Encoding as E
import Network.Wai.Test (SResponse (SResponse))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Model.User as User
import Data.String

import Mock (appCtx)

spec :: Spec
spec = with (appCtx >>= app) $ do
        describe "E2E App Testing" $ do
                it "Login should redirect to login page" $ do
                        get "/login?service=http://example.com" `shouldRespondWith` 302 {matchHeaders = ["Location" <:> "/html/login.html?service=http%3A%2F%2Fexample.com"]}
                
                let extractTicket headers =
                        BS.drop (BS.length ticketP) . snd . BS.breakSubstring ticketP <$> lookup "Location" headers
                        where ticketP = "ticket="


                it "Test login form submission should return a valid ticket" $ do
                        let exId = "1234567890"
                            exFn = "John"
                            exLn = "Doe"

                        -- User login
                        respLogin <- postHtmlForm "/login" [
                                ("service", "http://example.com"),
                                ("ouid", exId),
                                ("firstname", exFn),
                                ("lastname",exLn)]

                        -- Retrieve ticket
                        let (SResponse (Status code _) headers _) = respLogin
                        liftIO $ code `shouldBe` 302
                        ticket <- case extractTicket headers of
                                        Just s -> return s
                                        Nothing -> liftIO $ expectationFailure ("No ticket Headers:" ++ show headers) >> pure "NOTHING"

                        -- Validate ticket
                        (SResponse _ _ j) <- request methodGet "serviceValidation" [
                                                ("DeeAppId", "APPID"),
                                                ("DeeAppSecret", "APPSECRET"),
                                                ("DeeTicket", ticket)] ""
                        case decode j of
                          Just user -> liftIO $ do
                                  User.ouid user `shouldBe` fromString exId
                                  User.firstname user `shouldBe` fromString exFn
                                  User.lastname  user `shouldBe` fromString exLn
                          _ -> liftIO . expectationFailure $ "Incorrect data from validation (not json object)"

                it "Redirect with correct parameter concatenation" $ do
                        let matchHeaderContainsTicket headers _ = 
                                case extractTicket headers of
                                        Just _ -> Nothing 
                                        Nothing  -> Just "No ticket"
                        postHtmlForm "/login" [
                                ("service", "http://example.com/login?test=hello&how=ok"),
                                ("ouid", "1234567890"),
                                ("firstname","John"),
                                ("lastname","Doe")] `shouldRespondWith`
                                        302 {matchHeaders  = [MatchHeader matchHeaderContainsTicket]}

                it "Login page redirect with parameter" $ do
                        -- http://www.example.com/?a=b&c&d
                        get "/login?service=http%3A%2F%2Fwww.example.com%2F%3Fa%3Db%26c%26d" `shouldRespondWith` 302 {matchHeaders = ["Location" <:> "/html/login.html?service=http%3A%2F%2Fwww.example.com%2F%3Fa%3Db%26c%26d"]}
                        
                it "Login with no service should return error" $ do
                        get "/login" `shouldRespondWith` 400

                it "Validating invalid ticket should error" $ do
                        request methodGet "/serviceValidation" [
                                                ("DeeAppId", "APPID"),
                                                ("DeeAppSecret", "APPSECRET"),
                                                ("DeeTicket", "123456")] "" `shouldRespondWith` 401
                it "Validating with wrong app credential should error" $ do
                        request methodGet "/serviceValidation" [
                                                ("DeeAppId", "APPIDJJJJJ"),
                                                ("DeeAppSecret", "APPSECRET"),
                                                ("DeeTicket", "123456")] "" `shouldRespondWith` 401




