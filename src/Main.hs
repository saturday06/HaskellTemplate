#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types.Status (statusCode)
import System.Environment
import Data.Function

main = do
  manager <- newManager defaultManagerSettings
  path <- getArgs & fmap (!! 0)
  url <- parseUrl "http://pyazo.office/upload.cgi"
  request <- url { method = "POST" } & formDataBody [partBS "id" "", partFileSource "imagedata" path]
  response <- httpLbs request manager
  print $ "The status code was: " ++ (response & responseStatus & statusCode & show)
  print $ responseBody response
