{-# LANGUAGE OverloadedStrings #-}

module App where

import Network.Wai as Wai
import Network.HTTP.Types (status200, status500) 
import Network.Wai.Handler.Warp as Warp
import Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Char8 as B8
import Data.Monoid ((<>))
import Data.List.Utils (replace)
import Data.Maybe (fromMaybe)
import System.Environment (getEnv, lookupEnv)

import Network.HTTP.Client as NC
import Network.URI (uriAuthority, uriRegName)

-- <> == mappend

rewritePage :: BL8.ByteString -> String -> String -> BL8.ByteString
rewritePage page localSite foreignSite = BL8.pack newText
    where
        replacedHostname = replace 
            foreignSite
            localSite
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

rerouteResponse :: Wai.Request -> String -> String -> IO Wai.Response
rerouteResponse originalRequest localSite foreignSite = do
    manager <- newManager defaultManagerSettings
    request <- buildRequest $ "http://" ++ foreignSite ++ (BL8.unpack $  fullPath originalRequest)
    response <- httpLbs request manager

    return $ Wai.responseLBS
        status200
        ([]) -- This should be replaced with some of php's response headers (ie. Content-Type) 
        (rewritePage 
            (NC.responseBody response)
            localSite
            foreignSite
        )

noEnvResponse :: String -> IO Wai.Response
noEnvResponse envName = return $ Wai.responseLBS
    status500
    ([])
    (BL8.pack $ envName ++ " environment variable not defined")

app :: Wai.Application
app request respond = do
    Prelude.putStrLn "I've done some IO here"

    maybeLocalSite <- lookupEnv "LOCAL_SITE"
    maybeForeignSite <- lookupEnv "FOREIGN_SITE"

    response <- case (maybeLocalSite, maybeForeignSite) of 
        (Just localSite, Just foreignSite) -> rerouteResponse request localSite foreignSite
        (Nothing, _) -> noEnvResponse "LOCAL_SITE"
        (_, Nothing) -> noEnvResponse "FOREIGN_SITE"

    respond response

run :: IO ()
run = do
    port <- getEnv "PORT"
    Prelude.putStrLn $ "http://localhost:" ++ port
    Warp.run (read 8080) app