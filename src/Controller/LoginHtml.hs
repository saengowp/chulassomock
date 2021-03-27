module Controller.LoginHtml (route) where

import Common
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty.Trans
import Model.MinUser

route :: AppScotty ()
route = do
        get "/html/login.html" $ do
                service <- param "service" `rescue` (\_ -> pure "http://www.example.com")
                let (MinimalUser sid fn ln)  = MinimalUser "6000000021" "John" "Doe"
                setHeader "Content-Type" "text/html; charset=UTF-8"
                (raw . renderHtml) $ H.docTypeHtml $ do
                        H.head $ do
                                H.title "Chula SSO Mock Login"
                        H.body $ do
                                H.h1 "Chula SSO Mock Login"
                                H.form H.! A.method "post" H.! A.action "/login" $ do
                                        H.input H.! A.type_ "hidden" H.! A.name "service" H.! A.value (H.textValue service)
                                        mapM_ (\(a, b) -> do
                                                H.div $ do
                                                        H.label (H.text a)
                                                        H.input H.! A.name (H.textValue a)  H.! A.value (H.textValue b)
                                                ) [
                                                        ("ouid", sid),
                                                        ("firstname", fn),
                                                        ("lastname", ln)
                                                        ]
                                        H.input H.! A.type_ "submit" H.! A.value "Login"
                                                




