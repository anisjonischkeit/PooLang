{-# LANGUAGE OverloadedStrings #-}
import Network.Wai as Wai -- (Application, Request, responseLBS, rawPathInfo, rawQueryString)
import Network.HTTP.Types (status200, status500) 
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 as BL8 --(fromStrict, ByteString, pack)
import Data.ByteString.Char8 as B8 --(pack)
import Data.Monoid ((<>))
import Data.List.Utils (replace)
import Data.Maybe (fromMaybe)
import System.Environment (getEnv, lookupEnv)

import Network.HTTP.Client as NC

-- <> == mappend

rewritePage :: BL8.ByteString -> BL8.ByteString
rewritePage page = BL8.pack newText
    where
        replacedHostname = replace 
            (BL8.unpack "php.net")
            (BL8.unpack "localhost:8080") 
            (BL8.unpack page)
        replacedLogo = replace 
            "<img src=\"/images/logos/php-logo.svg\" width=\"48\" height=\"24\" alt=\"php\">"
            "PooLang"
            replacedHostname
        newText = replace 
            "PHP"
            "PooLang"
            replacedLogo
            
buildRequest :: String -> IO NC.Request
buildRequest url = do
  nakedRequest <- parseRequest url
  return (nakedRequest { method = "GET" })


fullPath :: Wai.Request -> BL8.ByteString
fullPath request = 
    BL8.fromStrict $ 
        (Wai.rawPathInfo request) <> (Wai.rawQueryString request)

rerouteResponse :: Wai.Request -> String -> IO Wai.Response
rerouteResponse request site = do
    manager <- newManager defaultManagerSettings
    request <- buildRequest $ site ++ (BL8.unpack $  fullPath request)
    response <- httpLbs request manager
    
    return $ Wai.responseLBS
        status200
        ([])
        --(NC.responseHeaders response)
        (rewritePage $ NC.responseBody response)

noEnvResponse :: IO Wai.Response
noEnvResponse = return $ Wai.responseLBS
    status500
    ([])
    "REDIRECT_SITE environment variable not defined"

app :: Wai.Application
app request respond = do
    Prelude.putStrLn "I've done some IO here"

    redirectSite <- lookupEnv "REDIRECT_SITE"

    response <- case redirectSite of 
        Just site -> rerouteResponse request site
        Nothing -> noEnvResponse

    respond response


main :: IO ()
main = do
    Prelude.putStrLn $ "http://localhost:8080/"
    run 8080 app