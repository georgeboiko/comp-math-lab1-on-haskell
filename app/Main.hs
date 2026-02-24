{-# LANGUAGE OverloadedStrings #-}

module Main where

import Processors (processData)
import Web.Scotty
import Types.RequestTypes (Lab1InputData(..))
import Types.ResponseTypes

main :: IO ()
main = scotty 3000 $ do

    post "/api/lab/1" $ do
        requestData <-jsonData :: ActionM Lab1InputData
        payload <- liftIO $ processData requestData
        let info = Response { _status = "OK", _code = 200, _message = "Answer was calculated successfully" }
        let response = Lab1Response {_info = info, _payload = payload}
        json response

    get "/api/health/check" $ do
        json $ Response "OK" 200 "Server is healthy"