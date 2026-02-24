{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Types.RequestTypes (Lab1InputData(..), Lab1GenerateData(..))
import Types.ResponseTypes
import Processors.SimpleIterationsProcessors
import Utils.Generators

main :: IO ()
main = scotty 8000 $ do

    post "/api/lab/1" $ do
        requestData <-jsonData :: ActionM Lab1InputData
        payload <- liftIO $ processData requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab1Response {lab1Info = info, lab1Payload = payload}
        json response

    post "/api/lab/1/generate" $ do
        requestData <-jsonData :: ActionM Lab1GenerateData

        matrix <- liftIO $ generateMatrix (lab1GenN requestData) (lab1GenN requestData)
        vector <- liftIO $ generateVector (lab1GenN requestData)
        let inputData = Lab1InputData {
              lab1N = lab1GenN requestData
            , lab1Matrix = matrix
            , lab1Vector = vector
            , lab1Eps = lab1GenEps requestData 
            }
        payload <- liftIO $ processData inputData

        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab1Response {lab1Info = info, lab1Payload = payload}
        json response

    get "/api/health/check" $ do
        json $ Response "OK" 200 "Server is healthy"